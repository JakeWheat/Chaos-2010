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
> import Data.Word
> import System.Directory
> import Control.Monad
> import System.FilePath
> import Data.Maybe
> import Data.IORef
> import ChaosDB
> import System.Time
> import Text.Regex.Posix
> import qualified DBAdmin as DBAdmin
> import System.Environment
> import GtkUtils
> import qualified Conf as Conf
> import Utils
> import FileAdmin
> import Control.Concurrent
> import MyTextView
> import qualified DBTextView as D

================================================================================

= Main

Most of the bootstrapping is done in the windowManagerNew function

all that main needs to do is call this and call the two gtk init actions

> main :: IO ()
> main = do
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

>          unsafeInitGUIForThreadedRTS --initGUI
>          timeoutAddFull (yield >> return True)
>                         priorityDefaultIdle 100
>          withConn ("host=localhost dbname=" ++ Conf.dbName conf ++
>                    " user=" ++ Conf.username conf ++
>                    " password=" ++ Conf.password conf)
>            (\conn -> do
>            w <- windowManagerNew conn
>            mainGUI)

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
>   buf <- textViewGetBuffer tv
>   let refresh = do
>         textBufferClear buf
>         mapM_ (\items -> D.run conn items >>= render tv)
>               [turnPhaseInfo
>               ,spellInfo
>               ,cursorInfo
>               ,cursorPieces
>               ,selectedPieceInfo
>               ]
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
>           \tp -> [Text $ "turn_phase " ++ tp ++ "\t"]
>        ,D.Items [MyTextView.Button "continue" $ dbAction conn "next_phase" []]
>        ,D.SelectTupleIf "select current_wizard,colour,allegiance,sprite \n\
>                         \  from current_wizard_table\n\
>                         \  inner join allegiance_colours\n\
>                         \  on current_wizard = allegiance\n\
>                         \  natural inner join wizard_sprites;" [] $
>             \wi -> [
>                     Text "\nWizard up: "
>                    ,TaggedText (lk "current_wizard" wi)
>                                (filter (/="none") [lk "colour" wi])
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
>                                 (\(n,f) -> not (lk f sd == "" ||
>                                                 read (lk f sd) < 2))
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

>       pieceInfo pi = [
>         Text "\n"
>        ,sprite $ lk "sprite" pi] ++
>        (case True of
>           _ | lk "ptype" pi == "wizard" ->
>                 [TaggedText (lk "allegiance" pi) (case lk "colour" pi of
>                                                 "none" -> []
>                                                 s -> [s])]
>             | lk "dead" pi == "true" ->
>                 [TaggedText ("dead " ++ lk "ptype" pi ++ "-" ++ lk "tag" pi)
>                             ["grey"]]
>             | otherwise -> [
>                   Text (lk "ptype" pi ++ "-" ++ lk "tag" pi ++ "(")
>                  ,TaggedText (lk "allegiance" pi) (filter (/="none")
>                              [(lk "colour" pi)])
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
>                            ,"rideable"
>                            ,"imaginary"
>                            ,"shadow_form"
>                            ,"magic_sword"
>                            ,"magic_knife"
>                            ,"magic_shield"
>                            ,"magic_wings"
>                            ,"magic_armour"
>                            ,"magic_bow"
>                            ,"computer_controlled"]
>             pieceBoolStats = flip filter booleanStats (\s -> lk s pi == "true")
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
>            vals = zip atts $ map (\f -> lk f pi) atts
>            vals' = filter (\(f1,f2) -> f2 /= "") vals
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

= Board Widget

Use cairo, draw sprites which are held in png files. There are sprites
with multiple frames for each piece, and also sprites to represent
cursors and square highlights.

The sprites are loaded in the loadSprites function and made available
as cairo surfaces.

Each sprite has a set of animation frames.  The two animation styles
are:

* forward: start with first frame then second, third, etc. till end
  then loop to first frame again,

* forward_backward: as forward but instead of looping to first go to
  second last, third last, etc. until second frame then start loop
  again at first frame. This means that first and last frames are only
  played once in each loop. This isn't done yet

Each sprite starts animating on the first frame when it is created:
e.g. two different goblins won't necessarily be at the same frame,
makes it look a bit better.

The frames for a sprite change at different speeds according to the
sprite.

TODO: visual and sound effects

> boardWidgetNew :: Connection -> ColourList -> SpriteMap -> IO (Frame, IO())
> boardWidgetNew conn colours spriteMap = do
>     frame <- frameNew
>     canvas <- drawingAreaNew
>     containerAdd frame canvas

Not really sure about the following code, robbed it from a mixture of
places, just wanted to get something working for now.

The widget needs to be redrawn quite a lot from expose and resize and
events like that, but we only need to access the database when the
board has changed. So - read it in the ctor and then in the refresh
method, and only read this cached data when handling an expose event

>     bd <- readBoardSprites
>     boardData <- newIORef bd

use getFrames to tell us how many frames have passed since the app was
started, this is used to determine which frame of each sprite to show

>     startTime <- getClockTime
>     let getFrames = do
>           t <- getClockTime
>           let tdiff = diffClockTimes t startTime
>           --25 frames per second
>               ps = (tdPicosec tdiff * 25::Integer) `div` 10^12::Integer
>               f1 = fromIntegral (tdMin tdiff * 25 * 60) +
>                    fromIntegral (tdHour tdiff * 25 * 60 * 60) +
>                    fromIntegral (tdSec tdiff * 25) +  ps
>               b = fromInteger f1
>           return b

>     let myDraw w h = do
>         --make the background black
>         setSourceRGB 0 0 0
>         paint
>

setup some helper functions:

>         let boardWidth = 15
>             boardHeight = 10

work out the size of each square in cairo co-ords

>             squareWidth = (w / boardWidth) ::Double
>             squareHeight = (h / boardHeight) ::Double

create toX and toY functions, you pass these the square
position and it returns the drawing co-ords of the top
left of that square

>             toX a = fromIntegral a * squareWidth
>             toY b = fromIntegral b * squareHeight
>


>             drawGrid = do
>                        setSourceRGB 0.2 0.2 0.2
>                        --draw vertical gridlines
>                        mapM_ (\x -> do
>                              moveTo (toX x) 0
>                              lineTo (toX x) (toY 10)) [1..14]
>                        setLineWidth 1
>                        stroke
>                        --draw horizontal gridlines
>                        mapM_ (\y -> do
>                              moveTo 0 (toY y)
>                              lineTo (toX 15) (toY y)) [1..9]
>                        setLineWidth 1
>                        stroke
>
>         drawGrid
>
>

--------------------------------------------------
draw sprites

assume sprites are 64x64

>         let sw = 64
>             sh = 64
>

get our scale factors so that the sprites are drawn at the same size
as the grid squares

>             scaleX = squareWidth / fromIntegral sw
>             scaleY = squareHeight / fromIntegral sh
>

use a scale transform on the cairo drawing surface to scale the
sprites. This seems a bit backwards since we have to generate new toX
and toY functions which take into account the changed scale factor.

>         scale scaleX scaleY
>         let toXS a = fromIntegral a * squareWidth / scaleX
>             toYS b = fromIntegral b * squareHeight / scaleY
>

create a helper function to draw a sprite at board position x,y
identifying the sprite by name, hiding all that tedious map lookup
stuff

>         cf <- liftIO getFrames
>         let drawAt x y sp sf as = do
>                 let p = safeMLookup "board widget draw" sp spriteMap
>                 let (_,_,img) = p
>                 let f = ((cf - sf) `div` as) `mod` length img
>                 setSourceSurface (img !! f) (toXS x) (toYS y)
>                 paint

Draw the board sprites from the saved board

>         bd <- liftIO $ readIORef boardData
>         mapM_ (uncurry5 drawAt) bd

hook things up the the expose event

>     onExpose canvas
>              (\x -> do
>                        (w,h) <- widgetGetSize canvas
>                        drawin <- widgetGetDrawWindow canvas
>                        renderWithDrawable drawin
>                          (myDraw (fromIntegral w)
>                          (fromIntegral h))

The following line use to read
                         return (eventSent x))
but that doesn't compile anymore, so just bodged it.

>                        return True)
>
>     let redraw = do
>           win <- widgetGetDrawWindow canvas
>           region <- drawableGetClipRegion win
>           drawWindowInvalidateRegion win region True
>           drawWindowProcessUpdates win True

>     let refresh = do
>           --update the frame positions
>           f <- getFrames
>           dbAction conn "update_missing_startframes" [show f]
>           --update the ioref
>           bd <- readBoardSprites
>           writeIORef boardData bd
>           redraw

update the board sprites 10 times a second to animate them

>     flip timeoutAdd 100 $ do
>       widgetQueueDrawArea canvas 0 0 2000 2000
>       return True

>     return (frame, refresh)
>     where
>         readBoardSprites = do
>           br <- selectTuples conn "select * from board_sprites" []
>           return $ map (\bs -> (read $ lk "x" bs::Int,
>                                 read $ lk "y" bs::Int,
>                                 lk "sprite" bs,
>                                 read $ lk "start_frame" bs::Int,
>                                 read $ lk "animation_speed" bs::Int)) br



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
>   buf <- textViewGetBuffer tv
>   let refresh =
>         textBufferClear buf >>
>         D.run conn items >>= render tv
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
>   buf <- textViewGetBuffer tv
>
>   let refresh = do

first check the new_game_widget_state relvar

>         selectValueIfC conn "select count(*) from new_game_widget_state" []
>            (\c -> when (c == "0")
>              (dbAction conn "reset_new_game_widget_state" []))

>         textBufferClear buf
>         D.run conn items >>= render tv
>   return (tv, refresh)
>     where

Fill in the rows which correspond to each wizard

>       items = [D.SelectTuples "select * from new_game_widget_state\n\
>                              \order by line" [] $ \l ->

draw our sprite for this wizard

>                 [sprite $ lk "sprite" l
>                 ,Text $ "\t" ++ lk "sprite_name" l ++ "\t"

create a helper function for adding radio buttons as part of a group
these are the buttons which set whether a wizard is human, android,
or not present

>                 ,ToggleButtonGroup ["human", "computer", "none"]
>                                    (lk "state" l)
>                                    (\label -> runSql conn
>                                             "update new_game_widget_state\n\
>                                             \set state =? where line =?"
>                                             [label, lk "line" l])
>                 ,Text "\n"
>                 ]

add the buttons at the bottom of the panel to start the game and
reset the panel

>               ,D.Items $ [MyTextView.Button "start game" $
>                             dbAction conn
>                                      "client_new_game_using_\
>                                      \new_game_widget_state" []
>                             --temp:
>                             --win <- getParentWidget tv
>                             --widgetHideAll win
>                          ,MyTextView.Button "reset this window" $
>                             dbAction conn "reset_new_game_widget_state" []
>                          ,Text "\n"
>                          ] ++

add some temporary buttons to start custom games for testing purposes

>                           for ["all_pieces"
>                               ,"upgraded_wizards"
>                               ,"overlapping"
>                               ] (\l -> MyTextView.Button l $ do
>                                   dbAction conn
>                                            "client_new_game_using_\
>                                            \new_game_widget_state" []
>                                   callSp conn "setup_test_board" [l])
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
> actionHistoryWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   buf <- textViewGetBuffer tv

we save the id of the last history item shown. This is so when refresh
is called, it only needs to append the new ones to the bottom of the
text box instead of redrawing them all

>   lastHistoryIDBox <- newIORef (-1)

>   wizardColours <- selectLookup conn "select wizard_name,colour\n\
>                                      \from wizard_display_info" []
>   let getWC wn = fromMaybe "grey" $ lookup wn wizardColours
>       items lhID =
>             D.SelectTuplesIO "select * from action_history_mr\n\
>                           \where id > ?\n\
>                           \order by id" [show lhID] $
>               \h -> do
>                 t <- th h
>                 return $ t ++ [Text "\n"]
>       th h = do
>              writeIORef lastHistoryIDBox $ read $ lk "id" h
>              let wc = TaggedText (lk "wizard_name" h)
>                                  [getWC $ lk "wizard_name" h]
>                  pTag = [Text $ lk "ptype" h ++ "-"
>                         ,TaggedText (lk "allegiance" h)
>                                     [getWC $ lk "allegiance" h]
>                         ,Text $ '-' : lk "tag" h]
>              return $ (Text $ lk "id" h ++ ". ") :

-- unfinished...

>                case lk "history_name" h of
>                  "new game" ->
>                      [Text "new game started"]
>                  "choose spell" ->
>                      [wc
>                      ,TaggedText (" chose " ++ lk "spell_name" h) ["yellow"]]
>                  "next phase" ->
>                      [Text $ "next phase: " ++ lk "current_wizard" h ++
>                       " - " ++ lk "current_phase" h]
>                  "skip spell" ->
>                      [wc
>                      ,TaggedText (" skipped casting " ++ lk "spell_name" h)
>                                  ["yellow"]]
>                  "spell cast succeeded" ->
>                      [wc
>                      ,TaggedText (" successfully cast " ++ lk "spell_name" h)
>                                  ["green"]]
>                  "spell cast failed" ->
>                      [wc
>                      ,TaggedText (" failed to cast " ++ lk "spell_name" h)
>                                  ["red"]]
>                  "piece teleport" ->
>                      pTag ++ [Text " teleported"]
>                  "chinned" ->
>                      pTag ++ [Text " was chinned"]
>                  "shrugged off" ->
>                      pTag ++ [Text " shrugged off the attack"]
>                  "game won" ->
>                      [wc
>                      ,Text " has won!"]
>                  "game drawn" ->
>                      [Text "the game is a draw."]
>                  _ -> [Text $ lk "history_name" h ++ " FIXME"]

>       refresh = do
>         lhID <- readIORef lastHistoryIDBox
>         D.run conn [items lhID] >>= render tv
>         textViewScrollToBottom tv
>
>   return (tv, refresh)

================================================================================

= Window Manager Widget

This widget is provides a bunch of buttons which the user can toggle
to show the other windows. The program exits when this window is
closed.

TODO: save size, position, scroll positions, minimised/maximised, etc.

> windowManagerNew :: Connection -> IO TextView
> windowManagerNew conn =
>   handleSqlError $ do

== basic setup

load the colours and sprites, then create a textview to represent the
window manager widget

>     colours <- readColours conn
>     spriteMap <- loadSprites conn
>     wm <- myTextViewNew colours
>     buf <- textViewGetBuffer wm

Do the other widgets. Each line in the windows table corresponds to a top level
widget which is contained directly in a gtk window. For each line in the table,
create the widget and window if it isn't the window manager.

create a name for the window without underscores to
display to the user

>     let niceNameF = map
>                     (\c -> if c == '_' then ' ' else c)
>     selectValueIfC conn "select count(*) from windows\n\
>                      \where window_name = 'window_manager'" []
>                 (\c -> when (read c == 0)
>                    (dbAction conn "reset_windows" []))

== create windows

>     let castIt (iw, r) = return (castToWidget iw, r)

>     let makeWindow name = do
>         (widget,wrefresh) <- case name of
>           "info" ->
>             infoWidgetNew conn colours spriteMap >>= castIt
>           "board" ->
>             boardWidgetNew conn colours spriteMap >>= castIt
>           "spell_book" ->
>             spellBookWidgetNew conn colours spriteMap >>= castIt

We don't have the window manager's refresh at this state so
put in a dummy function and fix it up later

>           "window_manager" -> castIt (castToWidget wm, (return()))
>           "new_game" ->
>             newGameWidgetNew conn colours spriteMap >>= castIt
>           "action_history" ->
>             actionHistoryWidgetNew conn colours spriteMap >>= castIt
>           _ -> error ("unrecognised window name in windows relvar: " ++ name)

wrap each widget in a window

>         ww <- wrapInFullScroller widget >>=
>                        wrapInWindow (niceNameF name)

When closing the window manager, we close the database connection and
exit the app, when closing any of the other windows, we just hide the
window and update the database

>         if name == "window_manager"
>           then do
>             onDestroy ww mainQuit
>             return ()
>           else do
>             onDelete ww (const $ do
>               --hide the window and return true so that
>               -- gtk doesn't destroy the window
>               widgetHideAll ww
>               runSql conn
>                      "update windows set state='hidden'\n\
>                      \where window_name=?;" [name]
>               return True)
>             return ()

we save a list of the windows and refresh functions so that the window
manager refresh fn can hook the toggle buttons for each window up to
that window and hook pressing F12 up to refresh all the widgets

>         return (name, (ww, wrefresh))
>
>     v <- selectTuples conn "select window_name \n\
>                              \from windows" []
>     widgetData <- forM v (\r ->
>                   makeWindow (lk "window_name" r))

== refresh

Add a button for each widget which toggles whether the window for that
widget is visible or not (the windows can also be hidden by clicking
the close button also, clicking the window manager button is the only
way to unhide them).

>     let items = D.SelectTuplesIO "select window_name,px,py,sx,sy,state \n\
>                                \from windows" [] $ \wi -> do
>                   let name = lk "window_name" wi
>                       niceName = niceNameF name
>                       (ww,wrefresh) = safeLookup
>                                       "window manager refresh" name widgetData

now fix up the windows that contain the other widgets the window
manager handles all the window position, size and visibility so none
of the widgets have to worry about this when the resize and reposition
is hooked up to the database it will be hooked up in the window
manager ctor (this function)


>                   if name == "window_manager" || lk "state" wi /= "hidden"
>                     then widgetShowAll ww >> wrefresh
>                     else widgetHideAll ww
>                   --fix up the size and position
>                   windowMove ww (read $ lk "px" wi) (read $ lk "py" wi)
>                   windowResize ww (read $ lk "sx" wi) (read $ lk "sy" wi)

>                   return [ToggleButton niceName
>                            (lk "state" wi /= "hidden")

setup the handler so that clicking the button toggles the window visibility

>                            (\i -> if i
>                                  then do
>                                    widgetShowAll ww
>                                    --reposition the window since
>                                    --it's position is reset
>                                    -- when the window is hidden
>                                    windowMove ww (read $ lk "px" wi)
>                                                  (read $ lk "py" wi)
>                                    wrefresh
>                                    runSql conn
>                                      "update windows set state='normal'\n\
>                                      \where window_name=?;" [name]
>                                  else unless (name == "window_manager") $ do
>                                    widgetHideAll ww
>                                    runSql conn
>                                      "update windows set state='hidden'\n\
>                                      \where window_name=?;" [name])
>                          ,Text "\n"]
>         refresh = do
>           textBufferClear buf
>           D.run conn [items] >>= render wm

now we have our window manager refresh function, stick it into the
lookup which contains the widget and refresh functions

>     let widgetData' = updateLookup "window_manager"
>                         (fst $ safeLookup "window manager refresh widgets"
>                                           "window_manager" widgetData,
>                          refresh)
>                         widgetData
>
>     refresh
>
>

== Key press handling

>     let handleKeyPress e = do
>           case e of
>                  Key { eventKeyName = key } -> do
>                       --putStrLn ("Key pressed: " ++ key)
>                       dbAction conn "key_pressed" [key]


>                       when (key == "F12") $
>                          putStrLn "manual refresh" >>
>                          mapM_ (\(_,(_,r)) -> r) widgetData'

Until the notify stuff is working just do a full refresh after every
action as a kludge

>                       --mapM_ (\(_,(_,r)) -> r) widgetData'
>                       let (_,r) = fromJust $ lookup "board" widgetData'
>                       r

>                  _ -> error "key press handler got non key event"

>           return False

Add the handler to all the windows:

>     forM_ widgetData'
>           (\(_,(window,_)) ->
>                onKeyPress window handleKeyPress)

TODO: Hook up the relvar listener: need to toggle the buttons if the
database is altered

>     return wm


================================================================================

= sprite manager


> type SpriteMap = M.Map String ([Pixbuf], [Pixbuf], [Surface])

load sprites loads the pngs off the disk and
creates a pixbuf and mini pixbuf for use in the
text views and a cairo surface for drawing on the board

> loadSprites :: Connection -> IO SpriteMap
> loadSprites conn = do
>   maybeSpriteFiles <- findAllFiles "sprites"
>   spriteNames <- selectSingleColumn conn "select sprite from sprites" []
>   --for now, we just find the first matching first frame for a sprite
>   --since we aren't doing any animation
>   let spriteFilenames = for spriteNames
>         (\sp ->
>           let spritefiles = (filter
>                 (\l -> takeFileName l =~ ("^" ++ sp ++ "\\.[0-9]\\.png"))
>                 maybeSpriteFiles)
>           in if null spritefiles
>                then error $ "no sprite files for: " ++ sp
>                else sort spritefiles)
>   spritePixbufs <- mapM (\l -> mapM pixbufNewFromFile l) spriteFilenames

>   let eachPb fn = mapM (\l -> mapM fn l) spritePixbufs
>   --create the mini pixbufs
>   miniSpritePixbufs <- eachPb (\p -> pixbufScaleSimple p 16 16 InterpHyper)
>   --create the normal sized pixbufs
>   mediumSpritePixbufs <- eachPb (\p -> pixbufScaleSimple p 32 32 InterpHyper)
>   --create the cairo surfaces
>   spriteSurfaces <- forM spriteFilenames
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

> type ColourList = [(String,Color)]

> readColours :: Connection -> IO ColourList
> readColours conn = do
>   r <- selectTuples conn "select name,red,green,blue from colours" []
>   return $ map (\t -> ((lk "name" t),
>                      Color (read (lk "red" t))
>                            (read (lk "green" t))
>                            (read (lk "blue" t)))) r

Setup a text view with the styles and colours used in this app.

> myTextViewNew :: ColourList -> IO TextView
> myTextViewNew colours = do
>   tv <- textViewNew
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

>   tb <- textViewGetBuffer tv
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
>           where intToHex i =
>                    let h = showHex (truncate (fromIntegral i / 256)) ""
>                    in if length h < 2
>                         then '0' : h
>                         else h

> lk :: String -> M.Map String String -> String
> lk k = fromMaybe "" . M.lookup k
