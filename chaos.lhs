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

create some shortcuts

>   let ibc = textBufferInsertAtCursor buf
>       ibct = textBufferInsertAtCursorWithTags buf
>       ibs sn = textBufferInsertSpriteAtCursor buf sn spriteMap
>       iw = textViewInsertWidgetAtCursor tv
>       sv q = selectValueIf conn q []
>       st q = selectTupleIf conn q []
>       sts q = selectTuples conn q []

redraw the contents

>
>       refresh = do
>         textBufferClear buf
>         drawTurnPhaseInfo
>         drawSpellInfo
>         drawCursorInfo
>         drawCursorPieces
>         drawSelectedPieceInfo
>

== Turn phase info:
shows turn number, world alignment, turn phase, wizard up, continue button
if move phase, say how many pieces are left to move
TODO: add some text to tell the player what options he has or what he is
supposed to be doing

>       drawTurnPhaseInfo = do
>         sv "select * from turn_number_table"
>                         (\tn -> ibc ("Turn " ++ tn ++ ", "))
>         sv "select format_alignment(world_alignment) as alignment\n\
>                               \from world_alignment_table"
>                         (\wa -> ibc ("world alignment " ++ wa ++ ", "))
>         sv "select turn_phase from turn_phase_table"
>                     (\tp -> ibc ("turn_phase " ++ tp ++ "\t"))
>         but <- buttonNewWithLabel "continue"
>         onClicked but (dbAction conn "next_phase" [])
>         iw but
>         st "select current_wizard,colour,allegiance \n\
>                          \from current_wizard_table\n\
>                          \inner join allegiance_colours\n\
>                          \on current_wizard = allegiance"
>                          (\wi -> do
>                             ibc "\nWizard up: "
>                             ibct (wi "current_wizard")
>                                  (filter (/="none") [wi "colour"])
>                             ibs $ wi "allegiance")
>         sv "select turn_phase from turn_phase_table"
>              (\tp -> when (tp == "move")
>                        (sv "select count(*) from pieces_to_move"
>                        (\ptm -> ibc $ "\nPieces left to move: " ++ ptm)))

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

>       drawSpellInfo =
>         st "select * from current_wizard_selected_spell_details"
>                     (\sd -> do
>                        ibc $ "\nChosen spell: " ++
>                                sd "spell_name" ++ "\t"
>                        ibs $ sd "sprite"
>                        ibc $ "\n(" ++
>                             sd "spell_category" ++ ", " ++
>                             sd "alignment_string" ++ ", copies " ++
>                             sd "count" ++ ")" ++
>                             "\n" ++ sd "description" ++
>                             "\nchance " ++ sd "chance" ++ "% " ++
>                             " (base " ++ sd "base_chance" ++ "%)"
>                        --draw the extra spell casting info
>                        -- i think this code has got a bit stale
>                        -- and needs looking at again to check the
>                        --fields and field names
>                        let fields = flip filter [("#pieces", "num"),
>                                                  ("parts", "parts"),
>                                                  ("range", "range")]
>                                          (\(n,f) -> not (sd f == "" ||
>                                                     read (sd f) < 2))
>                        when (length fields > 0)
>                             (ibc $ '\n' : intercalate ", "
>                                      (for fields
>                                       (\(n,f) ->
>                                        n ++ ": " ++ sd f))))
>

== cursor info
shows the cursor position
and piece info (see below) for each piece
on the square the cursor is on

>       drawCursorInfo =
>         st "select x,y from cursor_position"
>                (\cp -> do
>                   ibc "\n"
>                   ibc "---------\n"
>                   ibc $ "\ncursor: " ++ cp "x" ++ ", " ++ cp "y")


== draw piece info helper

factor out this code since we want to draw piece info
for the selected piece and any pieces (could be up to three)
on the square the cursor is on

>       drawPieceInfo pi = do
>         ibc "\n"
>         ibs $ pi "sprite"
>         case True of
>           _ | pi "ptype" == "wizard" ->
>                 ibct (pi "allegiance") (filter (/="none") [(pi "colour")])
>             | pi "dead" == "true" ->
>                 ibct ("dead " ++ pi "ptype" ++ "-" ++ pi "tag")
>                      ["grey"]
>             | otherwise -> do
>                 ibc (pi "ptype" ++ "-" ++ pi "tag" ++ "(")
>                 ibct (pi "allegiance") (filter (/="none") [(pi "colour")])
>                 ibc ")"

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

>         let booleanStats = ["flying",
>                             "undead",
>                             "rideable",
>                             "imaginary",
>                             "shadow_form",
>                             "magic_sword",
>                             "magic_knife",
>                             "magic_shield",
>                             "magic_wings",
>                             "magic_armour",
>                             "magic_bow",
>                             "computer_controlled"]
>         let pieceBoolStats = flip filter booleanStats (\s -> pi s == "true")
>         when (length pieceBoolStats > 0)
>                  (ibc $ '\n' : intercalate ", " pieceBoolStats)
>         forM_ ["remaining_walk",
>                "move_phase",
>                "attack_strength",
>                "physical_defense",
>                "speed",
>                "agility",
>                "ranged_weapon_type",
>                "range",
>                "ranged_attack_strength",
>                "magic_defense",
>                "place"] (\f ->
>                          when (pi f /= "")
>                               (ibc $ "\n" ++ f ++ ": " ++ pi f))
>         return ()


== selected piece info
only displayed if there is a selected piece (in move phase)
show piece info for selected piece
which subphase the piece is in
squares left to move if walker and in moving subphase

>       drawSelectedPieceInfo =
>         st "select * from selected_piece_details"
>            (\pd -> do
>               ibc "\n"
>               ibc "--------\n"
>               ibc "\nSelected piece:"
>               drawPieceInfo pd)

== piece info subsubwidget
shows stats for the piece from:
pieces and sub entity tables
wizard upgrades

>       drawCursorPieces =
>         sts "select * from cursor_piece_details order by sp"
>             (\cpd -> do
>                ibc "\n"
>                ibc "-------\n"
>                drawPieceInfo cpd)

>   return (tv, refresh)


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
>           br <- selectRelation conn "select * from board_sprites" []
>           return $ map (\bs -> (read (bs "x")::Int,
>                                 read (bs "y")::Int,
>                                 bs "sprite",
>                                 read $ bs "start_frame"::Int,
>                                 read $ bs "animation_speed"::Int)) br



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
>   tv <-myTextViewNew colours
>   buf <- textViewGetBuffer tv
>   let ibc = textBufferInsertAtCursor buf
>       ibct = textBufferInsertAtCursorWithTags buf
>       ibs = (\sn -> textBufferInsertMiniSpriteAtCursor buf sn spriteMap)
>       sv q = selectValueIf conn q []
>       st q = selectTupleIf conn q []
>       sts q = selectTuples conn q []
>       refresh = do
>         textBufferClear buf

controls help text

>         sv "select * from spell_book_show_all_table"
>                (\s ->
>                 ibc $ (if s == "True"
>                          then "hide unavailable spells - DELETE"
>                          else "show all spells - INSERT") ++
>                     "\n0 - unselect spell")

write out the spells with spell category headers, use an ioref to keep
track of which header we are in for now - ugly and wrong, but it does
the job

>         spellCat <- newIORef "nowt"
>         sts "select * from spell_book_table\n\
>            \order by section_order, alignment_order,\n\
>            \base_chance desc, spell_name"
>            (\sb -> do
>               sc <- readIORef spellCat

see if we need to write a new category header

>               when (sc /= sb "spell_category") $ do
>                     ibc "\n"
>                     ibc "-------------"
>                     ibc $ "\n" ++ sb "spell_category" ++ " spells:"
>                     writeIORef spellCat (sb "spell_category")

write this spell's line

>               ibc "\n"
>               ibct (sb "key" ++ " - ") [sb "colour"]
>               ibs $ sb "sprite"
>               ibct (" " ++ sb "spell_name" ++
>                     " " ++ sb "chance" ++ "%") [sb "colour"]
>               ibc $ " " ++ sb "align_icons" ++ " " ++ sb "count_icons")
>
>   return (tv, refresh)

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
>   let ibc = textBufferInsertAtCursor buf
>       ibct = textBufferInsertAtCursorWithTags buf
>       ibcw = textViewInsertWidgetAtCursor tv
>       ibs = (\sn -> textBufferInsertSpriteAtCursor buf sn spriteMap)
>       sv q = selectValueIf conn q []
>       st q = selectTupleIf conn q []
>       sts q = selectTuples conn q []
>
>   let refresh = do
>         textBufferClear buf

so we don't end up with the
first check the new_game_widget_state relvar
the only way of exiting the program is to close the window manager
window, if the windows table is empty, this window won't show
and the program will be unexitable, this handles part of that

>         sv "select count(*) from new_game_widget_state"
>            (\c -> when (c == "0")
>                     (dbAction conn "reset_new_game_widget_state" []))

Now fill in the rows which correspond to each wizard

>         sts "select * from new_game_widget_state\n\
>             \order by line" (\l -> do

draw our sprite for this wizard

>                  ibs $ l "sprite"
>                  ibc $ "\t" ++ l "sprite_name" ++ "\t"

create a helper function for adding radio buttons as part of a group
these are the buttons which set whether a wizard is human, android,
or not present

>                  let insertRadio group label = do
>                      r <- case group of
>                             Nothing -> radioButtonNewWithLabel label
>                             Just g -> radioButtonNewWithLabelFromWidget
>                                         g label
>                      toggleButtonSetMode r False
>                      textViewInsertWidgetAtCursor tv r

when a button is clicked then update the new_game_widget_state relvar

>                      onToggled r $ do
>                        active <- toggleButtonGetActive r
>                        when active $ do
>                          label <- buttonGetLabel r
>                          runSql conn
>                            "update new_game_widget_state\n\
>                            \set state =? where line =?"
>                            [label, l "line"]
>                      return r

use out helper functions to add the three radio buttons for this line/wizard

>                  r1 <- insertRadio Nothing "human"
>                  r2 <- insertRadio (Just r1) "computer"
>                  r3 <- insertRadio (Just r1) "none"
>                  case l "state" of
>                    "human" -> toggleButtonSetActive r1 True
>                    "computer" -> toggleButtonSetActive r2 True
>                    "none" -> toggleButtonSetActive r3 True
>                  ibc "\n"
>                  return ())

add the buttons at the bottom of the panel to start the game and
reset the panel

>         sgb <- buttonNewWithLabel "start game"
>         onClicked sgb $ do
>           dbAction conn
>             "client_new_game_using_new_game_widget_state" []
>           --temp:
>           win <- getParentWidget sgb
>           widgetHideAll win
>         ibcw sgb
>         srb <- buttonNewWithLabel "reset this window"
>         onClicked srb $ dbAction conn "reset_new_game_widget_state" []
>         ibcw srb
>         ibc "\n"

add some temporary buttons to start custom games for testing purposes

>         forM_ ["all_pieces", "upgraded_wizards", "overlapping"]
>           (\b -> do
>             but <- buttonNewWithLabel b
>             onClicked but $ do
>               dbAction conn "client_new_game_using_new_game_widget_state" []
>               callSp conn "setup_test_board" [b]
>             ibcw but)
>         return ()
>   return (tv, refresh)

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

>   let ibc = textBufferInsertAtCursor buf
>       ibct = textBufferInsertAtCursorWithTags buf
>       ibs = (\sn -> textBufferInsertSpriteAtCursor buf sn spriteMap)
>       st t = selectTupleIf conn t []
>       sts t = selectTuples conn t []

>   wizardColours <- selectLookup conn "select wizard_name,colour\n\
>                                      \from wizard_display_info" []
>   let getWC wn = fromMaybe "grey" $ lookup wn wizardColours

>   let insertHistory h = do
>         ibc $ h "id" ++ ". "
>         let icw = ibct (h "wizard_name") [getWC $ h "wizard_name"]
>             doPTag h = do
>               ibc $ h "ptype" ++ "-"
>               ibct (h "allegiance") [getWC $ h "allegiance"]
>               ibc $ '-' : h "tag"

unfinished...

>         case h "history_name" of
>           "new game" ->
>               ibc "new game started"
>           "choose spell" ->
>               icw >> ibct (" chose " ++ h "spell_name") ["yellow"]
>           "next phase" ->
>               ibc $ "next phase: " ++ h "wizard_name" ++ h "current_phase"
>           "skip spell" ->
>               icw >> ibct (" skipped casting " ++ h "spell_name") ["yellow"]
>           "spell cast succeeded" ->
>               icw >> ibct (" successfully cast " ++ h "spell_name") ["green"]
>           "spell cast failed" ->
>               icw >> ibct (" failed to cast " ++ h "spell_name") ["red"]
>           "piece teleport" ->
>               doPTag h >> ibc " teleported"
>           "chinned" ->
>               doPTag h >> ibc " was chinned"
>           "shrugged off" ->
>               doPTag h >> ibc " shrugged off the attack"
>           "game won" ->
>               icw >> ibc " has won!"
>           "game drawn" ->
>               ibc "the game is a draw."
>           _ -> ibc $ h "history_name" ++ " FIXME"
>         return ()
>   let refresh = do
>         lastHistoryID <- readIORef lastHistoryIDBox
>         sts ("select * from action_history_mr\n\
>                           \where id > " ++ show lastHistoryID ++
>                           " order by id")
>             (\h -> do
>                    insertHistory h
>                    ibc "\n"
>                    writeIORef lastHistoryIDBox (read $ h "id"))
>
>         textViewScrollToBottom tv
>
>   refresh
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
>     selectValueIf conn "select count(*) from windows\n\
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
>     v <- selectRelation conn "select window_name \n\
>                              \from windows" []
>     widgetData <- forM v (\r ->
>                   makeWindow (r "window_name"))

== refresh

Add a button for each widget which toggles whether the window for that
widget is visible or not (the windows can also be hidden by clicking
the close button also, clicking the window manager button is the only
way to unhide them).

>     let refresh = do
>         textBufferClear buf
>         selectTuples conn "select window_name,px,py,sx,sy,state \n\
>                           \from windows" [] (\wi -> do
>               let name = wi "window_name"
>               --first add the buttons to the window manager widget
>               let niceName = niceNameF name
>               but <- toggleButtonNewWithLabel niceName
>               textViewInsertWidgetAtCursor wm but
>               textBufferInsertAtCursor buf "\n"
>               toggleButtonSetActive but $ wi "state" /= "hidden"
>               let (ww,wrefresh) = safeLookup
>                                     "window manager refresh" name widgetData

setup the handler so that clicking the button toggles the window visibility

>               onClicked but $ do
>                     active <- toggleButtonGetActive but
>                     if active
>                       then do
>                         widgetShowAll ww
>                         --reposition the window since it's position is reset
>                         -- when the window is hidden
>                         windowMove ww (read $ wi "px") (read $ wi "py")
>                         wrefresh
>                         runSql conn
>                                "update windows set state='normal'\n\
>                                \where window_name=?;" [name]
>                       else unless (name == "window_manager") $ do
>                           widgetHideAll ww
>                           runSql conn "update windows set state='hidden'\n\
>                                       \where window_name=?;" [name]

now fix up the windows that contain the other widgets the window
manager handles all the window position, size and visibility so none
of the widgets have to worry about this when the resize and reposition
is hooked up to the database it will be hooked up in the window
manager ctor (this function)

>               if name == "window_manager" || wi "state" /= "hidden"
>                 then widgetShowAll ww >> wrefresh
>                 else widgetHideAll ww
>               --fix up the size and position
>               windowMove ww (read $ wi "px") (read $ wi "py")
>               windowResize ww (read $ wi "sx") (read $ wi "sy"))
>

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

>                       mapM_ (\(_,(_,r)) -> r) widgetData'

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
>   r <- selectRelation conn "select name,red,green,blue from colours" []
>   return $ map (\t -> ((t "name"),
>                      Color (read (t "red"))
>                            (read (t "green"))
>                            (read (t "blue")))) r

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
