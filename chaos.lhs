#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

= Overview
introduction
main
info widget
board widget
spell book widget
new game widget
action history widget
window manager widget
sprite manager - loads the pngs from disk
my text view - some additional functions for working with text views
utils


= Introduction

This is the code to hook the client pl/pgsql code up to gtk.

to produce an executable try:
ghc --make -o chaos chaos.lhs

== widget summary
Window_manager - lists windows
New game - view and change the options when starting a new game
Board - shows the board and the pieces on it
Info - shows info/stats on one or more pieces
spells - view spell list for wizard or entire spell list, details for
each spell, choose spell

== Error handling:

no thought has been put into the error handling, some bits of code try
to muddle through (bits that read from the database to display) and
others force the whole app to quit.

== control summary

escape to exit program

ctrl-space to continue, uses space at the moment but this is a bit
easy to hit

num pad and cursor keys to move cursor

choose phase:
  letter to select spell
  end to select no spell
cast phase
  num 5 or enter to cast/ select target
  end to cancel or finish spell
move
  num 5 or enter to
    select piece
    select flying target
    select ranged target
  end to unselect piece

mouse (TODO)
click continue to continue
click buttons in window manager to hide/show windows or close windows
click exit in window manager to exit/ close window manager window
choose:
  click spell to select
  click no spell to select done
cast
  click anywhere for non target spell
  click target for targeted spell
  right click to cancel
move
  click piece to select
  click square to move
  click enemy to attack
  click target to fly/ range attack
  right click to unselect


================================================================================

> import Graphics.UI.Gtk hiding (disconnect)
> import Graphics.Rendering.Cairo
> import Graphics.UI.Gtk.Gdk.Events
>
> import Data.List
> import qualified Data.Map as M
> import qualified Data.Char as DC
> import Numeric
> import Control.Monad
> import System.FilePath
> import Data.Maybe
> import Data.IORef
> import Text.Regex.Posix
> import System.Environment
> import Control.Concurrent

> import ChaosDB
> import qualified DBAdmin as DBAdmin
> import GtkUtils
> import qualified Conf as Conf
> import Utils
> import FileAdmin
> import MyTextView
> import qualified DBTextView as D
> import qualified Logging
> import SoundLib
> import BoardWidget
> import ChaosTypes
> import ThreadingUtils

================================================================================

= Main

Most of the bootstrapping is done in the windowManagerNew function

all that main needs to do is call this and call the two gtk init actions

> main :: IO ()
> main = do
>  Logging.setupLogging
>  conf <- Conf.getConfig
>  args <- getArgs
>  case True of
>    _ | (length args == 1 && head args == "reset") ->
>            time (DBAdmin.reset conf)
>      | (length args == 1 && head args == "switch") ->
>            time (DBAdmin.switchOverTempDb conf)
>      | (length args == 1 && head args == "setup") ->
>            time (DBAdmin.setup conf)
>      | (length args == 1 && head args == "checkSprites") ->
>            time checkSprites
>      | (length args == 1 && head args == "dos") ->
>            time convertToDos >>
>            putStrLn "converted text files to dos line endings"
>      | (length args == 1 && head args == "unix") ->
>            time convertToUnix >>
>            putStrLn "converted text files to unix line endings"
>      | not (null args) ->
>            putStrLn "Call with no arguments to run game,\n\
>                     \or run 'chaos setup' to initialise database\n\n\
>                     \For developers: with exactly one of these \
>                     \arguments:\n\
>                     \reset\treset database from sql\n\
>                     \switch\tswitch temp db\n\
>                     \checkSprites\tcheck sprite pngs\n\
>                     \dos\tconvert text files (sql,lhs) to dos line endings\n\
>                     \unix\tconvert text files to unix line endings\n"
>      | otherwise -> do

todo: if cannot connect to database give info to this effect
 if database doesn't appear to be a chaos db or is empty, message

>          lg "initGui" "" unsafeInitGUIForThreadedRTS
>          timeoutAddFull (yield >> return True)
>                         priorityDefaultIdle 50
>          withConn ("host=localhost dbname=" ++ Conf.dbName conf ++
>                    " user=" ++ Conf.username conf ++
>                    " password=" ++ Conf.password conf)
>            (\conn -> do
>              setupGui conn
>              mainGUI)

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)


================================================================================

= Info Widget

Displays the following information:
turn phase
spell info
cursor info
piece info for pieces on current square
piece info for selected piece

> infoWidgetNew :: Connection -> ColourList -> SpriteMap -> IO (TextView, IO ())
> infoWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   fk <- forkAndQueueOneNew
>   let refresh = lg "infoWidgetNew.refresh" "" $ do
>         forkItemReplace fk conn tv "infoWidgetNew.refresh" $
>                    concat [turnPhaseInfo
>                           ,spellInfo
>                           ,cursorInfo
>                           ,cursorPieces
>                           ,selectedPieceInfo
>                           ]
>   return (tv, refresh)
>     where

== Turn phase info:
shows turn number, world alignment, turn phase, wizard up, continue button
if move phase, say how many pieces are left to move
TODO: add some text to tell the player what options he has or what he is
supposed to be doing

>       turnPhaseInfo = [

>         D.SelectValueIf "select * from turn_number_table" [] $
>           \tn -> [Text $ "Turn " ++ tn ++ ", "]

>        ,D.SelectValueIf "select format_alignment(world_alignment)\n\
>                         \    as alignment\n\
>                         \  from world_alignment_table" [] $
>           \wa -> [Text $ "world alignment " ++ wa ++ ", "]

>        ,D.SelectValueIf "select turn_phase from turn_phase_table" [] $
>           \tp -> [Text $ "turn_phase " ++ tp ++ "\n"]
>        ,D.SelectTuples "select help from prompt" [] $
>           \tp -> [Text $ lk "help" tp ++ "\n"]
>        --,D.Items [MyTextView.Button "continue" $ dbAction conn "next_phase" []]
>        ,D.SelectTupleIf "select current_wizard,colour,allegiance,sprite \n\
>                         \  from current_wizard_table\n\
>                         \  inner join allegiance_colours\n\
>                         \  on current_wizard = allegiance\n\
>                         \  natural inner join wizard_sprites;" [] $
>             \wi -> [
>                     Text "\nWizard up: "
>                    ,TaggedText (lk "current_wizard" wi)
>                                [lk "colour" wi]
>                    ,sprite $ lk "sprite" wi
>                    ]

>        ,D.SelectValueIf "select count from\n\
>                         \    (select count(*) from pieces_to_move) as a\n\
>                         \cross join turn_phase_table\n\
>                         \where turn_phase='move';" [] $
>                         \ptm -> [Text $ "\nPieces left to move: " ++ ptm]
>        ]

== spell info:
only displayed if the current wizard has selected a spell
 (either in the choose phase or cast phase)
list info about the spell (where applicable):
name
spell sprite
type
implementation type
base chance
current chance
alignment
description
range
max pieces summoned
ptype of pieces summoned - same as sprite
number of shots with spell

in casting phase, if spell has more than one part (server or client side)
then this widget show how many parts remain to be cast

>       spellInfo = [

>         D.SelectTupleIf
>           "select * from current_wizard_selected_spell_details" [] $
>           \sd -> [
>               Text $ "\nChosen spell: " ++
>                    lk "spell_name" sd ++ "\t"
>              ,sprite $ lk "sprite" sd

>              ,Text $ "\n(" ++
>                             lk "spell_category" sd ++ ", " ++
>                             lk "alignment_string" sd ++ ", copies " ++
>                             lk "count" sd ++ ")" ++
>                             "\n" ++ lk "description" sd ++
>                             "\nchance " ++ lk "chance" sd ++ "% " ++
>                             " (base " ++ lk "base_chance" sd ++ "%)"

draw the extra spell casting info:
i think this code has got a bit stale and needs looking at again to
check the fields and field names

>              ,let fields = flip filter [("#pieces", "num")
>                                        ,("parts", "parts")
>                                        ,("range", "range")
>                                        ]
>                                 (\(_,f) -> not (lk f sd == "" ||
>                                                 read (lk f sd) < (2::Int)))
>               in Text $ '\n' : intercalate ", "
>                                  (for fields
>                                       (\(n,f) ->
>                                        n ++ ": " ++ lk f sd))
>              ]
>        ]

== cursor info
shows the cursor position
and piece info (see below) for each piece
on the square the cursor is on

>       cursorInfo = [
>         D.SelectTupleIf "select x,y from cursor_position" [] $
>                \cp -> [Text $ "\n\
>                               \---------\n\
>                               \\ncursor: " ++ lk "x" cp ++ ", " ++ lk "y" cp]
>        ]


== draw piece info helper

factor out this code since we want to draw piece info
for the selected piece and any pieces (could be up to three)
on the square the cursor is on

>       pieceInfo pit = [
>         Text "\n"
>        ,sprite $ lk "sprite" pit] ++
>        (case True of
>           _ | lk "ptype" pit == "wizard" ->
>                 [TaggedText (lk "allegiance" pit) [lk "colour" pit]]
>             | lk "dead" pit == "true" ->
>                 [TaggedText ("dead " ++ lk "ptype" pit ++ "-" ++ lk "tag" pit)
>                             ["grey"]]
>             | otherwise -> [
>                   Text (lk "ptype" pit ++ "-" ++ lk "tag" pit ++ "(")
>                  ,TaggedText (lk "allegiance" pit)
>                              [lk "colour" pit]
>                  ,Text ")"]) ++

boolean stats are treated differently:
Don't display bool stats if they are false,
for all the stats which are true, display a csv list of their names
when multiplayer/ computer controlled:
TODO: hide imaginary? or make it optional:
if you are playing against the computer or other players over the network,
you might want to be able to see which of your monsters are imaginary
if you are playing with multiple players at one computer, you definitely
don't want to be able to see which monsters are imaginary (as with the
original chaos), cos then everyone can cheat. You should never be able
to see imaginary of monsters that aren't yours.

>        [let booleanStats = ["flying"
>                            ,"undead"
>                            ,"ridable"
>                            ,"imaginary"
>                            ,"shadow_form"
>                            ,"magic_sword"
>                            ,"magic_knife"
>                            ,"magic_shield"
>                            ,"magic_wings"
>                            ,"magic_armoxur"
>                            ,"magic_bow"
>                            ,"computer_controlled"]
>             pieceBoolStats = filter (\s -> lk s pit == "true") booleanStats
>         in Text $ '\n' : intercalate ", " pieceBoolStats
>        ] ++

>        let atts = ["remaining_walk"
>                   ,"move_phase"
>                   ,"attack_strength"
>                   ,"physical_defense"
>                   ,"speed"
>                   ,"agility"
>                   ,"ranged_weapon_type"
>                   ,"range"
>                   ,"ranged_attack_strength"
>                   ,"magic_defense"
>                   ,"place"]
>            vals = zip atts $ map (\f -> lk f pit) atts
>            vals' = filter (\(_,f2) -> f2 /= "") vals
>        in map (\(f1,f2) -> Text $ "\n" ++ f1 ++ ": " ++ f2) vals'


== selected piece info
only displayed if there is a selected piece (in move phase)
show piece info for selected piece
which subphase the piece is in
squares left to move if walker and in moving subphase

>       selectedPieceInfo =
>         [D.SelectTupleIf "select * from selected_piece_details" [] $
>            \pd -> (Text "\n\n\
>                        \--------\n\
>                        \\nSelected piece:" : pieceInfo pd)]

== piece info subsubwidget
shows stats for the piece from:
pieces and sub entity tables
wizard upgrades

>       cursorPieces =
>         [D.SelectTuples "select * from cursor_piece_details order by sp" [] $
>            \cpd -> (Text "\n\
>                        \-------\n" : pieceInfo cpd)]

>       sprite s = let (pb,_,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb

================================================================================

= Spell book widget

Basic idea

The spell book is a vertical widget with one line for each spell.

The spells always appear in the same order and are arranged by spell
type (wizard, attacking, object, misc, monster)

You can toggle whether you see all spells in the game with the ones
you don't have available to cast greyed out, or just see the spells
you have available.

Each spell line is:
Spell key - spell sprite spell name casting chance alignment number
e.g.
G - [W] magic wood 80% + #
this means press G to select magic wood, you have 80% chance of
success, it's alignment is law-1 and you have one copy
o - [O] orc 100% *  ##
this means you press o to select orc, you have 100% chance of
success, it's alignment is chaos-1 and you have 2 copies

almost all the work for this is done in the sql code

each spell you have available is coloured according to the spell cast
chance - the colours are from the original chaos:

white 100
yellow 90
cyan 70-80
green 50-60
purple 30-40
red 10-20

> spellBookWidgetNew :: Connection ->
>                       ColourList ->
>                       SpriteMap ->
>                       IO (TextView, IO())
> spellBookWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   fk <- forkAndQueueOneNew
>   let refresh = lg "spellBookWidgetNew.refresh" "" $
>         forkItemReplace fk conn tv "spellBookWidgetNew.refresh" items
>   return (tv, refresh)
>     where


show help text

>       items = [D.SelectValueIf "select * from spell_book_show_all_table" [] $
>                     \s -> [Text $ (if s == "True"
>                                    then "hide unavailable spells - DELETE"
>                                    else "show all spells - INSERT") ++
>                                           "\n0 - unselect spell"]
>               ,D.SelectTuples "select * from spell_book_table\n\
>                               \order by section_order, alignment_order,\n\
>                               \base_chance desc, spell_name" [] $

write this spell's line

>                  \sb -> [Text "\n"
>                         ,TaggedText (lk "key" sb ++ " - ") [lk "colour" sb]
>                         ,sprite $ lk "sprite" sb
>                         ,TaggedText (" " ++ lk "spell_name" sb ++
>                                       " " ++ lk "chance" sb ++ "%")
>                                     [lk "colour" sb]
>                         ,Text $ " " ++ lk "align_icons" sb ++ " " ++
>                            lk "count_icons" sb
>                         ]
>               ]

see if we need to write a new category header - todo: use this somehow

 >       writeLine sc sb = [Text $ "\n\
 >                                 \-------------\n\
 >                                 \\n" ++ sb "spell_category" ++
 >                                   " spells:" | sb "spell_category" /= sc]

>       sprite s = let (_,pb,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb

================================================================================

= New Game Widget

Starting new game involves the following choices:
number of wizards (2-8)
computer wizards same ai same stats as player
for each wizard:
    name text - autogenerated, can be changed
    computer_controlled bool
    sprite and colour displayed but cannot currently be changed

new game widget:
-------------------------------------------------------------
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
-------------------------------------------------------------

> newGameWidgetNew :: Connection ->
>                     ColourList ->
>                     SpriteMap ->
>                     IO (TextView, IO())
> newGameWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   fk <- forkAndQueueOneNew
>   let refresh = lg "newGameWidgetNew.refresh" "" $
>         forkUpdate fk "newGameWidgetNew.refresh"
>           (do
>             --first check the new_game_widget_state relvar
>             c <- selectValue conn "select count(*) from new_game_widget_state" []
>             when (c == "0")
>                  (dbAction conn "reset_new_game_widget_state" [])
>             D.run conn (items refresh))
>           (\i -> do
>              buf <- textViewGetBuffer tv
>              textBufferClear buf
>              render tv i)
>   return (tv, refresh)
>     where

Fill in the rows which correspond to each wizard

>       items refresh =
>               [D.SelectTuples "select * from new_game_widget_state\n\
>                              \order by line" [] $ \l ->

draw our sprite for this wizard

>                 [sprite $ lk "sprite" l
>                 ,Text $ "\t" ++ lk "sprite_name" l ++ "\t"

create a helper function for adding radio buttons as part of a group
these are the buttons which set whether a wizard is human, android,
or not present

>                 ,ToggleButtonGroup ["human", "computer", "none"]
>                                    (lk "state" l)
>                                    (\label -> forkIt $ runSql conn
>                                             "update new_game_widget_state\n\
>                                             \set state =? where line =?"
>                                             [label, lk "line" l])
>                 ,Text "\n"
>                 ]

add the buttons at the bottom of the panel to start the game and
reset the panel

>               ,D.Items $ [MyTextView.Button "start game" $
>                             forkIt $ dbAction conn
>                                      "client_new_game_using_\
>                                      \new_game_widget_state" []
>                             --temp:
>                             --win <- getParentWidget tv
>                             --widgetHideAll win
>                          ,MyTextView.Button "reset this window" $ forkIt $ do
>                             dbAction conn "reset_new_game_widget_state" []
>                             refresh
>                          ,Text "\n"
>                          ] ++

add some temporary buttons to start custom games for testing purposes

>                           for ["all_pieces"
>                               ,"upgraded_wizards"
>                               ,"overlapping"
>                               ] (\l -> MyTextView.Button l $ forkIt $ do
>                                   dbAction conn
>                                            "client_new_game_using_\
>                                            \new_game_widget_state" []
>                                   dbAction conn "setup_test_board" [l])
>                                   -- need to call refresh all here somehow
>               ]
>       sprite s = let (pb,_,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb

TODO: make this into a game manager which handles managing multiple
games and deleting ones you don't want, as well as starting new games

================================================================================

= Action history Widget

This shows a log of all the actions that have happened in a game, so
you can see what's going on.

> actionHistoryWidgetNew :: Connection ->
>                           ColourList ->
>                           SpriteMap ->
>                           IO (TextView, IO())
> actionHistoryWidgetNew conn colours _ = do
>   tv <- myTextViewNew colours
>   textViewAddScrollToBottom tv

we save the id of the last history item shown. This is so when refresh
is called, it only needs to append the new ones to the bottom of the
text box instead of redrawing them all

>   lastHistoryIDBox <- newIORef (-1::Integer)
>   fk <- forkAndQueueOneNew
>   let refresh = lg "actionHistoryWidgetNew.refresh" "" $ do
>         forkUpdate fk "actionHistoryWidgetNew.refresh"
>           (do
>            is <- items lastHistoryIDBox
>            D.run conn [is])
>           (\r -> do
>                  render tv r
>                  textViewScrollToBottom tv)
>
>   return (tv, refresh)
>     where
>       items lhIDBox = do
>             lhID <- readIORef lhIDBox
>             return $ D.SelectTuplesIO
>                           "select * from action_history_colour_mr\n\
>                           \where id > ?\n\
>                           \order by id" [show lhID] $
>               \h -> do
>                 writeIORef lhIDBox $ read $ lk "id" h
>                 return $ th h ++ [Text "\n"]
>       th h = let cTag = [lk "colour" h]
>                  wc = TaggedText (lk "allegiance" h) cTag
>                  ptype = [Text $ lk "ptype" h ++ "-"
>                          ,TaggedText (lk "allegiance" h) cTag
>                          ,Text $ '-' : lk "tag" h]
>              in (Text $ lk "id" h ++ ". ") :

-- unfinished...

>                case lk "history_name" h of
>                  "new_game" ->
>                      [Text $ "new game: " ++ lk "num_wizards" h ++
>                         " wizards are well up for a ruck"]
>                  "choose_spell" ->
>                      [wc
>                      ,TaggedText (" chose " ++ lk "spell_name" h) ["yellow"]]
>                  "wizard_up" ->
>                      [Text "wizard_up: "
>                      ,TaggedText (lk "allegiance" h) cTag
>                      ,Text (" - " ++ lk "turn_phase" h ++ " phase")]
>                  "spell_skipped" ->
>                      [wc
>                      ,TaggedText (" skipped casting " ++ lk "spell_name" h)
>                                  ["yellow"]]
>                  "spell_succeeded" ->
>                      [wc
>                      ,TaggedText (" successfully cast " ++ lk "spell_name" h)
>                                  ["green"]]
>                  "spell_failed" ->
>                      [wc
>                      ,TaggedText (" failed to cast " ++ lk "spell_name" h)
>                                  ["red"]]
>                  "chinned" ->
>                      ptype ++ [Text " was chinned"]
>                  "shrugged_off" ->
>                      ptype ++ [Text " shrugged off the attack"]
>                  "moved" ->
>                      ptype ++ [Text " moved"]
>                  "attack" ->
>                      ptype ++ [Text " attacked"]
>                  "new_turn" ->
>                      [Text $ "New turn: " ++ lk "turn_number" h]
>                  "game_won" ->
>                      [wc
>                      ,Text " has won!"]
>                  "game_drawn" ->
>                      [Text "the game is a draw."]
>                  _ -> [Text $ lk "history_name" h ++ " FIXME"]

                       "set_imaginary"
                       "set_real"
                       "spread"
                       "recede"
                       "disappear"

 >       writeText = mapM_ writeItem
 >       writeItem i = case i of
 >                     Text t -> putStr t
 >                     TaggedText t _ -> putStr t
 >                     _ -> return ()

================================================================================

= setup gui

This code creates the widgets and windows

> setupGui :: Connection -> IO ()
> setupGui conn = lg "setupGui" "" $ handleSqlError $ do
>   colours <- readColours conn
>   spriteMap <- loadSprites conn
>   player <- initPlayer

>   aiQF <- forkOneAtATimeNew

make sure the windows relvar is ok

>   c <- selectValue conn "select count(*) from windows\n\
>                         \where window_name = 'window_manager'" []
>   when (read c == (0::Int))
>        (dbAction conn "reset_windows" [])

create the windows and widgets

>   widgetData <- selectTuplesIO conn "select window_name,px,py,sx,sy,state\n\
>                                     \from windows" [] $
>                   \r -> do
>                         (name, (ww, wrefresh)) <- makeWindow aiQF
>                                                     colours player spriteMap
>                                                     (lk "window_name" r)
>                         showWindow r ww wrefresh
>                         return (name, (ww, wrefresh))

>   let refreshAll = mapM_ (\(_,(_,r)) -> r) widgetData

 >   let (_,refreshBoard) = safeLookup "get board refresh" "board" widgetData

== Key press handling

All the widgets/windows use the same key press handler, which mainly
sends the keycodes to the database code.

>   let handleKeyPress e = Logging.pLog "chaos.chaos.windowManagerNew.\
>                                         \handleKeyPress" "" $ do
>         case e of
>            Key { eventKeyName = key } -> do
>              forkIO $ lg "windowManagerNew.handleKeyPress.forkIO" "" $ do
>                --putStrLn ("Key pressed: " ++ key)
>                dbAction conn "key_pressed" [key]
>                when (key == "F12") refreshAll
>                --putStrLn "manual refresh" >> refresh
>                --Until the notify stuff is working just do a full
>                --refresh after every action as a kludge
>                --refreshAll

 >                flip idleAdd priorityDefaultIdle $ do
 >                  refreshBoard
 >                  return False

>            _ -> error "key press handler got non key event"
>         return False

Add the handler to all the windows:

>   forM_ widgetData (\(_,(window,_)) ->
>                     onKeyPress window handleKeyPress)
>   return ()

>   where
>     queueAiUpdate (fk, done) = do
>       fk $ do
>         ai <- selectValue conn "select count(1)\n\
>                                \  from valid_activate_actions\n\
>                                \   where action = 'ai_continue'" []
>         when ((read ai::Integer) /= 0) $ do
>           putStrLn "queue ai update"
>           threadDelay 1000000
>           putStrLn "running ai update"
>           dbAction conn "client_ai_continue_if" []
>           done
>           return ()
>       return ()

>     showWindow r ww wrefresh = do
>       widgetShowAll ww
>       windowMove ww (read $ lk "px" r) (read $ lk "py" r)
>       windowResize ww (read $ lk "sx" r) (read $ lk "sy" r)
>       wrefresh
>     castIt (iw, r) = return (castToWidget iw, r)
>     makeWindow aifk colours player spriteMap name = do
>       (widget,wrefresh) <- case name of
>           "info" ->
>             infoWidgetNew conn colours spriteMap >>= castIt
>           "board" ->
>             boardWidgetNew conn player colours spriteMap
>                            (queueAiUpdate aifk) >>= castIt
>           "spell_book" ->
>             spellBookWidgetNew conn colours spriteMap >>= castIt
>           "new_game" ->
>             newGameWidgetNew conn colours spriteMap >>= castIt
>           "action_history" ->
>             actionHistoryWidgetNew conn colours spriteMap >>= castIt
>           _ -> error ("unrecognised window name in windows relvar: " ++ name)

wrap each widget in a window

>       ww <- wrapInFullScroller widget >>=
>                        wrapInWindow name

>       onDestroy ww mainQuit

we save a list of the windows and refresh functions so that the window
manager refresh fn can hook the toggle buttons for each window up to
that window and hook pressing F12 up to refresh all the widgets

>       return (name, (ww, wrefresh))



================================================================================

= sprite manager

load sprites loads the pngs off the disk and
creates a pixbuf and mini pixbuf for use in the
text views and a cairo surface for drawing on the board

> loadSprites :: Connection -> IO SpriteMap
> loadSprites conn = lg "loadSprites" "" $ do
>   maybeSpriteFiles <- findAllFiles "sprites"
>   spriteNames <- selectSingleColumn conn "select sprite from sprites" []
>   let spriteFilenames = for spriteNames
>         (\sp ->
>           let spritefiles = (filter
>                 (\l -> takeFileName l =~ ("^" ++ sp ++ "\\.[0-9]\\.png"))
>                 maybeSpriteFiles)
>           in if null spritefiles
>                then error $ "no sprite files for: " ++ sp
>                else sort spritefiles)
>   spritePixbufs <- lg "loadfromfiles" "" $ mapM (\l -> mapM pixbufNewFromFile l) spriteFilenames
>   let eachPb fn = mapM (\l -> mapM fn l) spritePixbufs
>   --create the mini pixbufs
>   miniSpritePixbufs <- lg "createMinisprites" "" $ eachPb (\p -> pixbufScaleSimple p 16 16 InterpHyper)
>   --create the normal sized pixbufs
>   mediumSpritePixbufs <- lg "createNormalsprites" "" $ eachPb (\p -> pixbufScaleSimple p 32 32 InterpHyper)
>   --create the cairo surfaces
>   spriteSurfaces <- lg "createSurfaces" "" $ forM spriteFilenames
>     (\l -> mapM imageSurfaceCreateFromPNG l)
>   return $ M.fromList $
>            zip spriteNames $
>            zip3 mediumSpritePixbufs miniSpritePixbufs spriteSurfaces

robbed this from haskell-cafe, seems that keeping the surfaces created
directly from the png files is a bad idea so load them, then draw them
to another surface and keep that surface around.

> imageSurfaceCreateFromPNG :: FilePath -> IO Surface
> imageSurfaceCreateFromPNG file =
>     withImageSurfaceFromPNG file $ \png -> do
>         w <- renderWith png $ imageSurfaceGetWidth png
>         h <- renderWith png $ imageSurfaceGetHeight png
>         surf <- createImageSurface FormatARGB32 w h
>         renderWith surf $ do
>             setSourceSurface png 0 0
>             paint
>         return surf

================================================================================

= Utils

> readColours :: Connection -> IO ColourList
> readColours conn =
>   selectTuplesC conn "select name,red,green,blue from colours" []
>                 (\t -> ((lk "name" t),
>                         Color (read (lk "red" t))
>                               (read (lk "green" t))
>                               (read (lk "blue" t))))

Setup a text view with the styles and colours used in this app.

> myTextViewNew :: ColourList -> IO TextView
> myTextViewNew colours = do
>   tv <- textViewNew
>   tb <- textViewGetBuffer tv
>   textBufferInsertAtCursor tb "loading..."
>   textViewSetEditable tv False
>   textViewSetWrapMode tv WrapWord
>   fd <- fontDescriptionNew
>   fontDescriptionSetSize fd 12
>   fontDescriptionSetWeight fd WeightNormal
>   widgetModifyFont tv (Just fd)
>   widgetModifyText tv StateNormal (Color 0xcfff 0xcfff 0xcfff)
>   widgetModifyBase tv StateNormal (Color 0 0 0)

Setup the tags, we want one tag for each colour in the list of colours
passed to the ctor and one tag for the inverted colour (black text on
coloured background)

>   tagTable <- textBufferGetTagTable tb
>   forM_ colours (\(name, c) -> do
>     tag <- textTagNew (Just name)
>     set tag [textTagForeground := colourToHex c]
>     textTagTableAdd tagTable tag
>     inverseTag <- textTagNew (Just ("inverse-" ++ name))
>     set inverseTag [textTagBackground := colourToHex c,
>                     textTagForeground := "black"]
>     textTagTableAdd tagTable inverseTag
>     return ())

>
>   return tv


> textBufferInsertSpriteAtCursor :: TextBuffer -> String -> SpriteMap -> IO()
> textBufferInsertSpriteAtCursor tb spriteName spriteMap =
>   case M.lookup spriteName spriteMap of
>     Just (pb,_,_) -> textBufferInsertPixbufAtCursor tb (head pb)
>     _ -> return ()

> textBufferInsertMiniSpriteAtCursor :: TextBuffer ->
>                                       String ->
>                                       SpriteMap ->
>                                       IO()
> textBufferInsertMiniSpriteAtCursor tb spriteName spriteMap =
>   case M.lookup spriteName spriteMap of
>     Just (_,pb,_) -> textBufferInsertPixbufAtCursor tb (head pb)
>     _ -> return ()

== colorToHex

colourToHex: convert a GDK::Color to a html style #FFFFFF colour which
is what the textbuffer tags want to see when setting colours up, there
must be an easier way than this?

> colourToHex :: Color -> String
> colourToHex (Color red green blue) =
>           "#" ++ intToHex red ++ intToHex green ++ intToHex blue
>           where
>             intToHex i =
>                 let h = showHex ((div256 i)::Int) ""
>                 in if length h < 2
>                      then '0' : h
>                      else h
>             div256 i = truncate (fromIntegral i / 256::Double)

= temporary threading stuff

> forkIt :: IO() -> IO()
> forkIt a = forkIO a >> return()

> forkItemReplace :: Forker
>                 -> Connection
>                 -> TextView
>                 -> [Char]
>                 -> [D.Item]
>                 -> IO ()
> forkItemReplace fk conn tv logger items = do
>   forkUpdate fk logger
>              (D.run conn items)
>              (\i' -> do
>                      buf <- textViewGetBuffer tv
>                      textBufferClear buf
>                      render tv $ i')

> forkItemUpdate :: Forker
>                -> Connection
>                -> TextView
>                -> [Char]
>                -> [D.Item]
>                -> IO ()
> forkItemUpdate fk conn tv logger items = do
>   forkUpdate fk logger
>              (D.run conn items)
>              (\i' -> do
>                      render tv $ i')


> forkUpdate :: Forker -> String -> IO t -> (t -> IO ()) -> IO ()
> forkUpdate (fk,done) logger prepare rnder = do
>       fk $ lg (logger ++ ".prepare") "" $ do
>         x <- prepare
>         flip idleAdd priorityDefaultIdle $ lg (logger ++ ".render") "" $ do
>              rnder x
>              done
>              return False
>         return ()
>       return ()

 >     else prepare >>= rnder

Plan to get responsiveness fixed

Two main areas where the responsiveness is not good enough:

1) moving the cursor around

2) trying to click a button or quit whilst the ai is running

TODO: get some logging to highlight how long it takes to respond to
each event, also how long the gtk thread is updating instead of
waiting for the user - percentages, distribution of times.

Some ideas on how to fix these issues:

1) only have one update for each of the widgets running at one
time. If another update is needed and the current running update is
not in the render phase, kill it (need to be careful of problems
whilst the idle handler is being setup, and also check to see if
killing the update thread doesn't break an already setup idle handler

2) don't use this for the board widget, which must update as quickly
as possible. Never cancel a board widget update, but only ever have
one pending. To speed it up, kill and requeue any non board updates
running when an update board widget request is queued.

3) when anything changes, start the board widget update as soon as
possible, but wait 1/2 a second or something for the other widgets. if
another change comes in before the 1/2 second is up, reset it to wait
another 1/2 second - this might starve the other windows from being
updated though, maybe only do this if the update is a cursor move, or
maybe make them wait only if a board update is running, and kill and
requeue them if they are running during a board update. Need to test
to see what kind of latencies we get.
