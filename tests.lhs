#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

= Overview

main - main function, which runs the tests

tests - basic tests
spell cast tests
move actions - tests for the actions in the move phase
autonomous - tests for actions in the autonomous phase

diagrams - ascii art type thing to help readability of tests
test helpers
setup game functions - used to set up a game before running a test

db update helpers
db read shortcuts

= Introduction

Most of the tests revolve around setting up a game, then running a few
actions and checking what happens in the database. The actions mostly
run by feeding letters to the handle key press action to simulate
someone actually using the ui, the main big hole in the tests is
checking the displayed data in the ui.

We try to get maximum effect for a minimal amount of tests, so there
is no great attempt at comprehensiveness.

In order to speed the run of tests up, some items which might live in
different tests are combined to reduce the amount of work needed,
e.g. testing that next_phase is automatically called when a wizard
finished casting his spell is combined with a test for casting a
spell. This is probably evil and wrong.

Speed: running all the tests takes ages for very little work, this
highlights how slow the code is. I'm waiting for postgresql 8.4 which
apparently has easy function profiling to find out why it's so
slow. Probably some materialised views are needed.

Current Todo planned for beta:
create aliases for the keypress calls
move subphase tests
move misc tests
line of sight
prompt
autonomous
loop through all spells
missing wizard upgrades
valid squares after all updates

== Notes for checking database updates

Want to check all the data tables after each action is run, so we keep
track of all the relvar updates and make sure all the relvars which
shouldn't change haven't changed.

data relvars plus notes

history: added to each action
prompt: changes depending on valid squares
alignment: may change after casting
pieces: usually checked directly using the diagram stuff
current wizard
game completed
pieces to move
selected piece
spell parts to cast
squares left to walk
turn number
turn phase
wizard spell choices
spell books
wizards
cast success checked
cursor pos - tested implicitly all the time

data relvars that are always empty outside of a transactions
(metaphorically speaking these would be put on the stack instead of
the heap if we could):

cast magic wood squares
cast alignment
creating new game
*_hack

data relvars to skip testing
piece starting frames
board size
new game widget state
spell books show all
test action overrides
wizard display info
windows

reduce testing work:

don't check relvars which are unchanged every action, just check their
value immediately after they've changed ore at start of test, and
check they are still the same immediately before they're changed or at
end of test.

Don't check data covered by database constraints (at some point want
to find a way of testing the database constraints themselves).


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
> import Test.HUnit
> import ChaosDB
> import Conf
> import System.Time
> import Utils
> import qualified Debug.Trace.Location as L
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Test.Framework.Providers.QuickCheck


================================================================================

= Main

Run all the tests.

> main = time $ do
>   conf <- getConfig
>   withConn ("host=localhost dbname=" ++
>             dbName conf ++ " user=" ++ username conf ++
>             " password=" ++ password conf) (\conn -> do
>     defaultMain [
>         testGroup "basics" [
>                        testDatabaseStuff conn
>                       ,testCursorMovement conn
>                       ,testPiecesOnTop conn
>                       ]
>        ,testGroup "phases" [
>                        testNextPhase conn
>                       ,testNextPhaseWizardDead conn
>                       ,testNextPhaseTwoWizardsDead conn
>                       ]
>        ,testGroup "casting" [
>                        testCastGoblin conn
>                       ,testFailCastGoblin conn
>                       ,testCastMagicWood conn
>                       ,testCastShadowWood conn
>                       ,testCastMagicBolt conn
>                       ,testCastMagicBoltResisted conn
>                       ,testCastVegeanceWizard conn
>                       ,testCastVegeanceMonster conn
>                       ,testCastVegeanceMonsterResisted conn
>                       ,testCastSubversion conn
>                       ,testCastSubversionResisted conn
>                       ,testCastDisbelieveReal conn
>                       ,testCastDisbelieveImaginary conn
>                       ,testCastRaiseDead conn
>                       ,testCastArmour conn
>                       ,testCastLaw conn
>                       ,testImaginary conn
>                       ]
>        ,testGroup "move phase stuff" [
>           testGroup "move subphases" [
>                          testMoveSubphases1 conn
>                         ,testMoveSubphases2 conn
>                         ,testMoveSubphases3 conn
>                         ,testMoveSubphases4 conn
>                         ]
>          , testGroup "moving" [
>                          testWalkOneSquare conn
>                         ,testWalkTwoSquares conn
>                         ,testFlyOverPieces conn
>                         ]
>           ,testGroup "attacking" [
>                          testAttackMonster conn
>                         ,testAttackMonsterResisted conn
>                         ,testAttackWizard conn
>                         ,testFlyAttack conn
>                         ,testFlyThenAttack conn
>                         ,testRangedAttack conn
>                         ,testRangedAttackResisted conn
>                         ,testShadowWoodAttack conn
>                         ]
>           ,testGroup "mount,enter,etc." [
>                         testMoveWhenMounted conn
>                        ,testDismount conn
>                        ,testMoveWhenAlreadyMounted conn
>                        ,testEnter conn
>                        ,testExit conn
>                        ]
>           ]
>        ,testGroup "game complete" [
>                        testWizardWin conn
>                       ,testGameDraw conn
>                       ]
>                 ])

postgresql.conf  #track_functions = none # none, pl, all

 select * from pg_stat_user_functions ;
http://www.depesz.com/index.php/2008/05/15/waiting-for-84-function-stats/

================================================================================

= Tests

== tests in sql

First run the tests from the database, these are all the database
functions whose name starts with 'check_code_'

> testDatabaseStuff conn = testCase "testDatabaseStuff" $ do
>   res <- newIORef ([]::[(String,Bool)])
>   selectTuples conn "select object_name\n\
>                     \from module_objects\n\
>                     \where object_name ~ 'check_code_.*'\n\
>                     \and object_type='operator';" []
>                    (\t -> do
>      v <- selectValue conn ("select " ++  t "object_name" ++ "()")
>      r1 <- readIORef res
>      writeIORef res $ r1 ++
>                     [(t "object_name", read v::Bool)])
>   r2 <- readIORef res
>   let r3 = filter (\(s,b) -> not b) r2

>   when (length r3 > 0)
>      (assertBool ("sql tests failed: " ++
>                   intercalate "\n" (map fst r3)) False)

>   return ()


== pieces on top

check our pieces on top logic.

Refresher:
corpses cannot be interacted with if there is another piece
(you can interact with corpses by disbelieve, raise dead, also
 possibly ranged attack, bolt, vengeance...?)
wizards can't be interacted with when they are in a castle or magic tree
or riding a monster
monsters cannot be interacted with if there is a gooey blob on the square
here is a list of the combos, with the interactable piece last:
a corpse monster
b corpse wizard
c corpse wizard monster
d wizard monster
e wizard castle
f wizard magic tree
g corpse gooey
h monster gooey
i corpse monster gooey

TODO: we can usually only interact with the top piece on each square,
but there are exceptions: mounted wizard who hasn't moved, wizard in
magic tree or castle; add test for these.

> testPiecesOnTop conn = testCase "testPiecesOnTop" $ do
>   startNewGame conn
>   setupBoard conn ("\n\
>                   \b      c      d\n\
>                   \               \n\
>                   \ aghi          \n\
>                   \               \n\
>                   \e             f\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                    [('a', [PieceDescription "goblin" "Kong Fuzi" [],
>                            PieceDescription "elf" "dead" []]),
>                     ('b', [PieceDescription "goblin" "dead" [],
>                            PieceDescription "wizard" "Buddha" []]),
>                     ('c', [PieceDescription "pegasus" "Kong Fuzi" [],
>                            PieceDescription "giant" "dead" [],
>                            PieceDescription "wizard" "Kong Fuzi" []]),
>                     ('d', [PieceDescription "gryphon" "Laozi" [],
>                            PieceDescription "wizard" "Laozi" []]),
>                     ('e', [PieceDescription "magic_castle" "Moshe" [],
>                            PieceDescription "wizard" "Moshe" []]),
>                     ('f', [PieceDescription "magic_tree" "Muhammad" [],
>                            PieceDescription "wizard" "Muhammad" []]),
>                     ('g', [PieceDescription "goblin" "dead" [],
>                            PieceDescription "gooey_blob" "Buddha" []]),
>                     ('h', [PieceDescription "goblin" "Kong Fuzi" [],
>                            PieceDescription "gooey_blob" "Buddha" []]),
>                     ('i', [PieceDescription "goblin" "Kong Fuzi" [],
>                            PieceDescription "gooey_blob" "Buddha" [],
>                            PieceDescription "elf" "dead" []])]))
>   checkTopPieces conn ("\n\
>                   \b      c      d\n\
>                   \               \n\
>                   \ aghi          \n\
>                   \               \n\
>                   \e             f\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                    [('a', [PieceDescription "goblin" "Kong Fuzi" []]),
>                     ('b', [PieceDescription "wizard" "Buddha" []]),
>                     ('c', [PieceDescription "pegasus" "Kong Fuzi" []]),
>                     ('d', [PieceDescription "gryphon" "Laozi" []]),
>                     ('e', [PieceDescription "magic_castle" "Moshe" []]),
>                     ('f', [PieceDescription "magic_tree" "Muhammad" []]),
>                     ('g', [PieceDescription "gooey_blob" "Buddha" []]),
>                     ('h', [PieceDescription "gooey_blob" "Buddha" []]),
>                     ('i', [PieceDescription "gooey_blob" "Buddha" []])]))


== cursor movement

Check the cursor movement and also the shortcut for the tests to move
the cursor to a given position, also check the moveto code

> testCursorMovement conn = testCase "testCursorMovement" $ do
>   --make sure there is a game running:
>   startNewGame conn
>   --reset the cursor position
>   runSql conn "update cursor_position set x=0,y=0;" []
>   let moveAndCheck m x y = do
>         sendKeyPress conn $ cursorShorthand m
>         checkCursorPosition conn x y
>   --move to our starting position then move in a circle

>   moveAndCheck "dr" 1 1
>   moveAndCheck "dr" 2 2
>   moveAndCheck "dr" 3 3
>   moveAndCheck "r" 4 3
>   moveAndCheck "ur" 5 2
>   moveAndCheck "u" 5 1
>   moveAndCheck "ul" 4 0
>   moveAndCheck "l" 3 0
>   moveAndCheck "dl" 2 1
>   moveAndCheck "d" 2 2
>   --now test the move cursor to straight up, left, down, right,
>   let moveToAndCheck x y = do
>           moveCursorTo conn x y
>           checkCursorPosition conn x y
>   moveToAndCheck 5 5
>   moveToAndCheck 5 1
>   moveToAndCheck 5 6
>   moveToAndCheck 3 6
>   moveToAndCheck 10 6
>   --upleft downleft  upright downright
>   moveToAndCheck 7 3
>   moveToAndCheck 4 6
>   moveToAndCheck 8 4
>   moveToAndCheck 12 8
>   --corners
>   moveToAndCheck 0 0
>   moveToAndCheck 14 0
>   moveToAndCheck 0 9
>   moveToAndCheck 14 9
>
>   --some random places around the board
>   --todo
>
>   return ()

start with a few helper functions

avoid writing out the full key press names:

> cursorShorthand :: String -> String
> cursorShorthand m = safeLookup "cursor shorthand" m
>                      [("d", "Down"),
>                       ("l", "Left"),
>                       ("u", "Up"),
>                       ("r", "Right"),
>                       ("dl", "KP_End"),
>                       ("dr", "KP_Page_Down"),
>                       ("ul", "KP_Home"),
>                       ("ur", "KP_Page_Up")]

get the current cursor position from the database

> readCursorPosition :: Connection -> IO (Int,Int)
> readCursorPosition conn = do
>   r <- selectRelation conn "select x,y from cursor_position" []
>   return (read $ head r "x", read $ head r "y")

move the cursor to x,y, using key presses

> moveCursorTo :: Connection -> Int -> Int -> IO ()
> moveCursorTo conn x y = do
>     --diagonals first then straight moves
>     (cx,cy) <- readCursorPosition conn
>     -- putStrLn $ "move " ++ (show (cx,cy)) ++ " to " ++ (show (x,y))
>     unless ((cx,cy) == (x,y)) $ do
>         let (dx,dy) = (x - cx, y - cy)
>             diagonalMoves = minimum [abs dx, abs dy]
>             diagonalDirection = case 0 of
>                                 _ | dx == 0 && dy == 0 -> "ul"
>                                   | dx < 0 && dy < 0 -> "ul"
>                                   | dx < 0 && dy > 0 -> "dl"
>                                   | dx > 0 && dy < 0 -> "ur"
>                                   | dx > 0 && dy > 0 -> "dr"
>         -- do it in two stages cos I'm not smart enough
>         sequence_ (replicate diagonalMoves
>                    (sendKeyPress conn $ cursorShorthand diagonalDirection))
>         (cx,cy) <- readCursorPosition conn
>         unless ((cx,cy) == (x,y)) $ do
>           let (dir,amount) = case 0 of
>                            _ | cx < x -> ("r", x - cx)
>                              | cx > x -> ("l", cx - x)
>                              | cy < y -> ("d", y - cy)
>                              | cy > y -> ("u", cy - y)
>                              | otherwise -> ("u", 0)
>           sequence_ (replicate amount
>                        (sendKeyPress conn $ cursorShorthand dir))

> checkCursorPosition conn x y = do
>   cp <- readCursorPosition conn
>   assertEqual "cursor position" cp (x, y)

> goSquare conn x y = do
>   moveCursorTo conn x y
>   sendKeyPress conn "Return"

== next phase

Just run through the choose, cast and move phases for each wizard
twice, check the turn_phase and current_wizard each time

> testNextPhase conn = testCase "testNextPhase" $ do
>   startNewGame conn
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                checkCurrentWizardPhase conn (wizardNames !! i) phase
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                tp <- readTurnPhase conn
>                when (tp == "choose")
>                     (sendKeyPress conn "Q")
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead conn = testCase "testNextPhaseWizardDead" $
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizardNames !! j]
>     let theseWizards = dropItemN wizardNames j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>         checkCurrentWizardPhase conn (theseWizards !! i) phase
>         --so we don't skip the cast phase, make sure
>         -- each wizard has a spell chosen, use disbelieve
>         --cos wizards always have this spell available
>         tp <- readTurnPhase conn
>         when (tp == "choose")
>              (sendKeyPress conn "Q")
>         sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead conn = testCase "testNextPhaseTwoWizardsDead" $
>   forM_ [0..7] (\j ->
>     forM_ [(j + 1)..7] (\k -> do
>       startNewGame conn
>       --kill wizards
>       callSp conn "kill_wizard" [wizardNames !! j]
>       callSp conn "kill_wizard" [wizardNames !! k]
>       let theseWizards = dropItemN (dropItemN wizardNames k) j
>       forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>         forM_ [0..5] (\i -> do
>           checkCurrentWizardPhase conn (theseWizards !! i) phase
>           --so we don't skip the cast phase, make sure
>           -- each wizard has a spell chosen, use disbelieve
>           --cos wizards always have this spell available
>           tp <- readTurnPhase conn
>           when (tp == "choose")
>                (sendKeyPress conn "Q")
>           sendKeyPress conn "space"))))


check wizards dying during move when it is their turn - this can
happen if you shoot your own wizard with a ranged weapon from a
monster, the game should cope with it - I think this is tested in the
game drawn test

automatic next phase tests:
casting the last part of a spell moves to the next player automatically
moving the last creature moves to the next player automatically
these are tested in the spell cast and move sections respectively


================================================================================

= spell cast tests

> testCastGoblin conn = testCase "testCastGoblin" $ do

setup the game, get to cast phase with the first wizard having
chosen goblin

>   startNewGame conn
>   addSpell conn "goblin"
>   sendKeyPress conn $ keyChooseSpell "goblin"
>   --get the next wizard to select disbelieve so we can check the
>   --auto next phase works
>   sendKeyPress conn "space"
>   sendKeyPress conn $ keyChooseSpell "disbelieve"
>   skipToPhase conn "cast"

check the squares we can cast goblin onto

>   checkValidSquares conn "\n\
>                      \1X     2      3\n\
>                      \XX             \n\
>                      \               \n\
>                      \               \n\
>                      \4             5\n\
>                      \               \n\
>                      \               \n\
>                      \               \n\
>                      \               \n\
>                      \6      7      8"

cast it and check the resulting board

>   rigActionSuccess conn "cast" True
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Buddha" []])]))
>   --check we are on the next wizard's phase
>   checkCurrentWizardPhase conn "Kong Fuzi" "cast"

> testFailCastGoblin conn = testCase "testFailCastGoblin" $ do
>   newGameReadyToCast conn "goblin"
>   rigActionSuccess conn "cast" False
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)

> testCastMagicWood conn = testCase "testCastMagicWood" $ do
>   newGameReadyToCast conn "magic_wood"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   checkBoard conn ("\n\
>                   \1W W W 2      3\n\
>                   \               \n\
>                   \W W W          \n\
>                   \               \n\
>                   \4W W          5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('W', [PieceDescription "magic_tree" "Buddha" []])]))

> testCastShadowWood conn = testCase "testCastShadowWood" $ do
>   newGameReadyToCast conn "shadow_wood"
>   checkValidSquares conn "\n\
>                          \1XXXXXX2X     3\n\
>                          \XXXXXXXXX      \n\
>                          \XXXXXXXXX      \n\
>                          \XXXXXXXX       \n\
>                          \4XXXXXXX      5\n\
>                          \XXXXXXX        \n\
>                          \XXXXXXX        \n\
>                          \XXXXX          \n\
>                          \XXX            \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   mapM_ (\(x,y) -> goSquare conn x y)
>         [(1,0),(3,0),(5,0),(0,2),(2,2),(4,2),(1,4),(3,4)]
>   checkBoard conn ("\n\
>                   \1W W W 2      3\n\
>                   \               \n\
>                   \W W W          \n\
>                   \               \n\
>                   \4W W          5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('W', [PieceDescription "shadow_tree" "Buddha" []])]))

> testCastMagicBolt conn = testCase "testCastMagicBolt" $ do
>   newGameReadyToCast conn "magic_bolt"
>   checkValidSquares conn "\n\
>                          \1      2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \X             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   goSquare conn 0 4
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \              5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)

> testCastMagicBoltResisted conn = testCase "testCastMagicBoltResisted" $ do
>   newGameReadyToCast conn "magic_bolt"
>   checkValidSquares conn "\n\
>                          \1      2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \X             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
>   goSquare conn 0 4
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)


> testCastVegeanceWizard conn = testCase "testCastVegeanceWizard" $ do
>   newGameWithBoardReadyToCast conn "vengeance"
>                  ("\n\
>                   \1G     2      3\n\
>                   \G              \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     X      X\n\
>                          \X              \n\
>                          \               \n\
>                          \               \n\
>                          \X             X\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \X      X      X"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   goSquare conn 7 0
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)

> testCastVegeanceMonster conn = testCase "testCastVegeanceMonster" $ do
>   newGameWithBoardReadyToCast conn "vengeance"
>                 ("\n\
>                   \1G     2      3\n\
>                   \G              \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     X      X\n\
>                          \X              \n\
>                          \               \n\
>                          \               \n\
>                          \X             X\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \X      X      X"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \G              \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))

> testCastVegeanceMonsterResisted conn =
>     testCase "testCastVegeanceMonsterResisted" $ do
>   newGameWithBoardReadyToCast conn "vengeance"
>                   ("\n\
>                   \1G     2      3\n\
>                   \G              \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     X      X\n\
>                          \X              \n\
>                          \               \n\
>                          \               \n\
>                          \X             X\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \X      X      X"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \G              \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))


> testCastSubversion conn = testCase "testCastSubversion" $ do
>   newGameWithBoardReadyToCast conn "subversion"
>                  ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \4             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Buddha" []])]))


> testCastSubversionResisted conn = testCase "testCastSubversionResisted" $ do
>   newGameWithBoardReadyToCast conn "subversion"
>                  ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \4             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))


> testCastDisbelieveReal conn = testCase "testCastDisbelieveReal" $ do
>   newGameWithBoardReadyToCast conn "disbelieve"
>                 ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   checkValidSquares conn "\n\
>                          \1X     2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \4             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))

> testCastDisbelieveImaginary conn = testCase "testCastDisbelieveImaginary" $ do
>   newGameWithBoardReadyToCast conn "disbelieve"
>                   ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi"
>                            [PImaginary]])]))
>   checkValidSquares conn "\n\
>                          \1X     2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \4             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)
>

> testCastRaiseDead conn = testCase "testCastRaiseDead" $ do
>   newGameWithBoardReadyToCast conn "raise_dead"
>                   ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "dead" []])]))
>   checkValidSquares conn "\n\
>                          \1X     2      3\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \4             5\n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \               \n\
>                          \6      7      8"
>   rigActionSuccess conn "cast" True
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Buddha" [PUndead]])]))


> testCastArmour conn = testCase "testCastArmour" $ do
>   newGameReadyToCast conn "magic_armour"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (updateLookup '1'
>                    [PieceDescription "wizard" "Buddha" [PArmour]]
>                    wizardPiecesList))


> testCastLaw conn = testCase "testCastLaw" $ do
>   newGameReadyToCast conn "law"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   dbAlign <- selectRelation conn "select world_alignment\n\
>                                  \from world_alignment_table" []
>   assertEqual "world alignment not law after casting law"
>              1 (read $ head dbAlign "world_alignment")


> testImaginary conn = testCase "testImaginary" $ do

>   let setStuffUp1 = do
>                     startNewGame conn
>                     addSpell conn "goblin"
>                     sendKeyPress conn $ keyChooseSpell "goblin"
>   let setStuffUp2 = do
>                     skipToPhase conn "cast"
>                     rigActionSuccess conn "cast" True
>                     goSquare conn 1 0
>   let sv c = do
>              v <- selectValue conn "select imaginary from monster_pieces\n\
>                                    \natural inner join pieces\n\
>                                    \where x= 1 and y = 0"
>              c ((read v) :: Bool)
>   setStuffUp1
>   setStuffUp2
>   sv (\v -> assertBool "default real for monsters" $ not v)

>   setStuffUp1
>   sendKeyPress conn "n"
>   setStuffUp2
>   sv (\v -> assertBool "cast real monster" $ not v)

>   setStuffUp1
>   sendKeyPress conn "y"
>   setStuffUp2
>   sv (\v -> assertBool "cast imaginary monster" v)

>   setStuffUp1
>   sendKeyPress conn "y"
>   sendKeyPress conn "n"
>   setStuffUp2
>   sv (\v -> assertBool "dither then cast real monster" $ not v)

>   setStuffUp1
>   sendKeyPress conn "n"
>   sendKeyPress conn "y"
>   setStuffUp2
>   sv (\v -> assertBool "dither then cast imaginary monster" v)
>

todo: wizard upgrades stats, usage in move tests


what are the side effects to check for when casting spells:
spell direct effect
spell book looses spell
history added
world alignment changed
turn phase changed
check it's the next wizards turn

> newGameReadyToCast conn spellName = do
>   startNewGame conn
>   addSpell conn spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase conn "cast"

> newGameWithBoardReadyToCast conn spellName board = do
>   startNewGame conn
>   setupBoard conn board
>   addSpell conn spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase conn "cast"

================================================================================

= move actions

== move subphases

Check that the game progresses through the subphases correctly, as
well as the actions work, the subphases are move, attack, ranged
attack, the program misses one out if it isn't relevant to a piece,
moves to the next subphase when the piece can do no more in its
current subphase, and unselects the piece automatically when it has no
more move subphases.


our subphase progression options for different pieces depending on
their capabilities are:


1 walk-done
2 walk-attack-done
3 walk-ranged-done
4 walk-attack-ranged-done
5 attack-done (for shadow wood)

Want to check variations where each action is used or cancelled,
substitute walk for fly in tests, and do variations for pieces which
can walk more than one square.

If we expand these out to switch in the cancel variations, fly and
walk one or more squares, we end up with way too many combinations,
need a better way to get reasonable coverage.

Hopefully, because of the database constraints, we can assume that
e.g. attacking then ranged attacking is the same whether we've previously
walked, flew or cancelled, so:

* we only need to cover each unique neighbouring pair of actions from
  the list above;

* we can test multiple pairs in one go e.g. walk-attack-ranged-done
  will cover walk-attack, attack-ranged and ranged-done;

* we can run through all cancel variations and not need to do any
  other combos with cancel. e.g. instead of:

** walk-attack-done
** cancel-attack-done
** walk-cancel-done
** cancel-cancel-done

with variations for flying and walking different amounts of squares,
we just need to to cancel-cancel-done to cover them all.

Same idea about database constraints goes for walking: creatures with
a one square walk work the same as creatures who have one square left
to walk, and that e.g. cancelling after one square of walk for a two
or more square walker, or two squares of walk for a three or more
square walker are the same, etc., so for each walk variation we need
one with a one square walker, one with a two square walker where we
cancel after the first square, and one with a flyer.

A piece can also move straight into an attack, e.g. a walker in the
moving subphase can select a neighbouring square an attack it without
having to cancel the moving subphase first, and a flying creature can
fly straight onto an enemy to attack it, so we need to test
walk-attack and fly-attack as well, with the same reasoning as before,
transitions from these attacks work the same as transitions from a
normal attack and so these don't need any additional transition
testing.

From the implementation we know that a walk-attack works the same
whether the piece has finished it's walk or not, so we don't need to
test walk-attack separately from normal attacking.

Final bits to take into account: the program skips the attack phase if
there are no attackable squares, moving and ranged-attacking are not
skipped automatically like this, so we want to test this skipping from
walk, walk-cancel and fly, skipping to both ranged attack and done
(but we don't need all the combinations)

Combine the subphase progression with testing the walk, fly, attack,
fly-attack, and ranged-attack variations.

Our final list of tests is

all cancel versions:

fly-attack-ranged attack
walk-attack
fly-ranged attack
walk
attack (for shadow wood)

no cancel versions:

fly-attack-ranged attack-done
flyattack-ranged attack
walk-attack-done
walkcancel-attack
skip-done (shadow wood)
walk-skip-ranged attack
fly-skip-done
walkcancel-skip-done

these should cover the subphase progression pretty well.

we have at least two attacks and two ranged attacks, make sure there
is at least one success and failure of each checked

see below for the misc move phase tests not included in these

TODO: implement this plan
Old move phase tests follow

> testMoveSubphases1 conn = testCase "testMoveSubphases1" $ do
>     --test 1: move -> attack -> ranged -> unselected
>     startNewGameReadyToMove conn ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    (wizardPiecesList ++
>                   [('G', [PieceDescription "golden_dragon" "Buddha" []]),
>                    ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])]))
>     --select dragon
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "golden_dragon"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "attack"
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "ranged-attack"
>     sendKeyPress conn "End"
>     --check nothing selected and dragon no longer in ptm table
>     checkNoSelectedPiece conn
>     v <- selectRelationValues conn
>                              "select * from pieces_to_move\n\
>                              \where ptype='golden_dragon' and\n\
>                              \  allegiance='Buddha'" []
>     assertBool "piece not in ptm" (null v)

> testMoveSubphases2 conn = testCase "testMoveSubphases2" $ do
>     --test 2: move -> ranged -> unselected>
>     startNewGameReadyToMove conn ("\n\
>                   \1G     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    (wizardPiecesList ++
>                   [('G', [PieceDescription "golden_dragon" "Buddha" []])]))
>     --select dragon
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "golden_dragon"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "ranged-attack"
>     sendKeyPress conn "End"
>     --check nothing selected and dragon no longer in ptm table
>     checkNoSelectedPiece conn
>     v <- selectRelationValues conn
>                              "select * from pieces_to_move\n\
>                              \where ptype='golden_dragon' and\n\
>                              \  allegiance='Buddha'" []
>     assertBool "piece not in ptm" (null v)
> testMoveSubphases3 conn = testCase "testMoveSubphases3" $ do
>     --test 3: move -> unselected
>     startNewGameReadyToMove conn ("\n\
>                   \1R     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    (wizardPiecesList ++
>                   [('R', [PieceDescription "giant_rat" "Buddha" []])]))
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "giant_rat"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     --check nothing selected and dragon no longer in ptm table
>     checkNoSelectedPiece conn
>     v <- selectRelationValues conn
>                              "select * from pieces_to_move\n\
>                              \where ptype='giant_rat' and\n\
>                              \  allegiance='Buddha'" []
>     assertBool "piece not in ptm" (null v)
> testMoveSubphases4 conn = testCase "testMoveSubphases4" $ do
>     --test 4: attack -> unselected
>     startNewGameReadyToMove conn ("\n\
>                   \1BR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    (wizardPiecesList ++
>                   [('B', [PieceDescription "bear" "Buddha" []]),
>                    ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])]))
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "bear"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "attack"
>     sendKeyPress conn "End"
>     --check nothing selected and dragon no longer in ptm table
>     checkNoSelectedPiece conn
>     v <- selectRelationValues conn
>                              "select * from pieces_to_move\n\
>                              \where ptype='bear' and\n\
>                              \  allegiance='Buddha'" []
>     assertBool "piece not in ptm" (null v)

== moving

> testWalkOneSquare conn = testCase "testWalkOneSquare" $ do
>     startNewGameReadyToMove conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList)
>     goSquare conn 0 0
>     goSquare conn 1 0
>     checkBoard conn ("\n\
>                   \ 1     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   wizardPiecesList)
>     --test that the next_phase has been automatically called since
>     --there is nothing left for this wizard to dothat we are on the
>     --next wizards move
>     checkCurrentWizardPhase conn "Kong Fuzi" "move"

> testWalkTwoSquares conn = testCase "testWalkTwoSquares" $ do
>     let pk = (wizardPiecesList ++
>                   [('B',[PieceDescription "bear" "Buddha" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1B     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   pk)
>     goSquare conn 1 0
>     --move down
>     goSquare conn 1 1
>     checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \ B             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   pk)
>     --move down-right
>     goSquare conn 2 2
>     checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \  B            \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   pk)

> testFlyOverPieces conn = testCase "testFlyOverPieces" $ do
>     let pk = (wizardPiecesList ++
>                   [('E', [PieceDescription "eagle" "Buddha" []]),
>                    ('G', [PieceDescription "goblin" "Buddha" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1E     2      3\n\
>                   \GGG            \n\
>                   \GGG            \n\
>                   \GGG            \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   pk)
>     goSquare conn 1 0
>     goSquare conn 3 4
>     checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \GGG            \n\
>                   \GGG            \n\
>                   \GGG            \n\
>                   \4  E          5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   pk)

== attacking

> testAttackMonster conn = testCase "testAttackMonster" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1G     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \ 1     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   updateLookup '1'
>                    [PieceDescription "wizard" "Buddha" [],
>                     PieceDescription "goblin" "dead" []]
>                    wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])])

> testAttackMonsterResisted conn = testCase "testAttackMonsterResisted" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1G     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" False
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \1G     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))


> testAttackWizard conn = testCase "testAttackWizard" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1G     2      3\n\
>                   \ H             \n\
>                   \               \n\
>                   \ H             \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('H', [PieceDescription "goblin" "Buddha" []])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   checkBoard conn ("\n\
>                   \G      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))

> testFlyAttack conn = testCase "testFlyAttack" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   G           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('E', [PieceDescription "eagle" "Buddha" []])]))
>   goSquare conn 2 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 3 3
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   E           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('E', [PieceDescription "eagle" "Buddha" [],
>                           PieceDescription "goblin" "dead" []])]))

> testFlyThenAttack conn = testCase "testFlyThenAttack" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   G           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('E', [PieceDescription "eagle" "Buddha" []])]))
>   goSquare conn 2 0
>   goSquare conn 2 3
>   rigActionSuccess conn "attack" True
>   goSquare conn 3 3
>   checkBoard conn ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   E           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('E', [PieceDescription "eagle" "Buddha" [],
>                           PieceDescription "goblin" "dead" []])]))

> testRangedAttack conn = testCase "testRangedAttack" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   G           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('E', [PieceDescription "elf" "Buddha" []])]))
>   goSquare conn 2 0
>   sendKeyPress conn "End"
>   rigActionSuccess conn "ranged_attack" True
>   goSquare conn 3 3
>   checkBoard conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   g           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('E', [PieceDescription "elf" "Buddha" []]),
>                    ('g', [PieceDescription "goblin" "dead" []])]))

> testRangedAttackResisted conn = testCase "testRangedAttackResisted" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   G           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('E', [PieceDescription "elf" "Buddha" []])]))
>   goSquare conn 2 0
>   sendKeyPress conn "End"
>   rigActionSuccess conn "ranged_attack" False
>   goSquare conn 3 3
>   checkBoard conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \   G           \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('E', [PieceDescription "elf" "Buddha" []])]))

== misc move phase stuff

check shadow tree winning fight doesn't move
killing wizard removes creations
mount, dismount, enter, exit, etc.


> testShadowWoodAttack conn = testCase "testShadowWoodAttack" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1W     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []]),
>                    ('W', [PieceDescription "shadow_tree" "Buddha" []])]))
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 1
>   checkBoard conn ("\n\
>                   \1W     2      3\n\
>                   \ g             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('W', [PieceDescription "shadow_tree" "Buddha" []]),
>                    ('g', [PieceDescription "goblin" "dead" []])])

== mounts

Test that the first time when try to select a square with a mounted
wizard we get the wizard, if we cancel then select again on that
square, we should get the monster the wizards is mounted on. To reduce
the number of tests we also test moving the monster whilst the wizard
is mounted here.

> testMoveWhenMounted conn = testCase "testMoveWhenMounted" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \ P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []])])
>   --select then cancel the wizard
>   goSquare conn 1 0
>   assertSelectedPiece conn "wizard" "Buddha"
>   sendKeyPress conn "End"
>   --now we can select the monster
>   sendKeyPress conn "Return"
>   assertSelectedPiece conn "pegasus" "Buddha"
>   goSquare conn 2 2
>   checkBoard conn ("\n\
>                   \       2      3\n\
>                   \               \n\
>                   \  P            \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []])])

dismount then move

> testDismount conn = testCase "testDismount" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \ P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []])])
>   goSquare conn 1 0
>   goSquare conn 1 1
>   goSquare conn 1 0
>   goSquare conn 3 0
>   checkBoard conn ("\n\
>                   \   P   2      3\n\
>                   \ 1             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "pegasus" "Buddha" []])])

move when already mounted

> testMoveWhenAlreadyMounted conn = testCase "testMoveWhenAlreadyMounted" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \ P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []])])
>   goSquare conn 1 0
>   sendKeyPress conn "End"
>   sendKeyPress conn "Return"
>   goSquare conn 3 0
>   checkBoard conn ("\n\
>                   \   P   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []])])

todo: attack when dismounting, dismounting when flying

== enter/exit

> testExit conn = testCase "testExit" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \ P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "dark_citadel" "Buddha" []])])
>   goSquare conn 1 0
>   assertSelectedPiece conn "wizard" "Buddha"
>   goSquare conn 1 1
>   checkBoard conn ("\n\
>                   \ P     2      3\n\
>                   \ 1             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "dark_citadel" "Buddha" []])])

> testEnter conn = testCase "testEnter" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('P', [PieceDescription "dark_citadel" "Buddha" []])]))
>   goSquare conn 0 0
>   goSquare conn 1 0
>   checkBoard conn ("\n\
>                   \ P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                    wizardPiecesList ++
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "dark_citadel" "Buddha" []])])

check when moving a mounted wizard, that the corpse on that square stays put

trying to walk/fly to occupied squares

shadow form attack
cobra attack dragon

test select one creature and move, then select a second creature and move

attack undead - unable
attack undead - able (undead on undead crime, and wizards with magic
  weapons), check results in no corpse

test monsters in blob cannot be selected, but can be
moved if free'd that turn
 - so check if in selected list, try to select, check failure, then
  kill blob and select and move

shadow form attack and moving, shadow form and magic wings at same time

engaged stuff

second monster dying on a square - the first corpse should disappear permanently

> startNewGameReadyToMove conn board = do
>   startNewGame conn
>   setupBoard conn board
>   skipToPhase conn "move"

================================================================================

= autonomous action tests

fire, blob spreading
castles disappearing
wizards getting new spell from magic tree

================================================================================

= winning/drawing

Win is tested when next phase is called following the original
chaos. We check that the win has been detected by seeing the win
history item in the history table, and by checking the valid activate
and target action views are empty.

> testWizardWin conn = testCase "testWizardWin" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1G     2       \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               ",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" []])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   sendKeyPress conn "space"
>   r <- selectValue conn "select count(1) from action_history_mr\n\
>                         \where history_name='game won';"
>   assertEqual "game won history entry" 1 (read r)
>   r <- selectValue conn "select count(1)\n\
>                         \from client_valid_target_actions;"
>   assertEqual "now valid target actions" 0 (read r)
>   r <- selectValue conn "select count(1)\n\
>                         \from client_valid_activate_actions;"
>   assertEqual "now valid activate actions" 0 (read r)

Draw works similarly to win, except it is detected the instant there
are no wizards left.

> testGameDraw conn = testCase "testGameDraw" $ do
>   startNewGameReadyToMove conn ("\n\
>                   \1G 2           \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               ",
>                   (wizardPiecesList ++
>                   [('G', [PieceDescription "green_dragon" "Kong Fuzi" []])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   --now the dragon shoots it's own wizard to force the draw
>   rigActionSuccess conn "ranged_attack" True
>   goSquare conn 3 0
>   r <- selectValue conn "select count(1) from action_history_mr\n\
>                         \where history_name='game drawn';"
>   assertEqual "game drawn history entry" 1 (read r)
>   r <- selectValue conn "select count(1)\n\
>                         \from client_valid_target_actions;"
>   assertEqual "now valid target actions" 0 (read r)
>   r <- selectValue conn "select count(1)\n\
>                         \from client_valid_activate_actions;"
>   assertEqual "now valid activate actions" 0 (read r)



================================================================================

= Diagrams

In the testing, we frequentlyu use a square list which is either
* a list of x,y co-ordinates
    used for testing valid squares for an action
* list of x,y co-ordinates each with a list of piece descriptions
    used to describe a board, either to set one up or
    to check against the board in the database

To help the tests be a bit more easy to write and understand, both of
this lists can be created with 'diagrams': a sort of acsii art layout
thingy.

The parseDiagram function converts the layout to a list of key,x,y
triples

This code is used both for the valid squares and the board diagrams.

> parseDiagram :: String -> [(Char,Int,Int)]
> parseDiagram board =
>     let ls' = lines board
>     in if head ls' /= ""
>          then error $ "first line in diagram should be empty string, got " ++
>                        head ls'
>          else
>            let ls = tail ls'
>            in case True of
>                  _ | length ls /= 10 ->
>                        error $ "diagram should have 10 lines, but has " ++
>                          show (length ls)
>                    | not $ null $ filter (\l -> length l /= 15) ls ->
>                        error "all lines after first must be \
>                              \15 chars in diagram"
>                    | otherwise ->
>                        let sqs = flip concatMap [0..9]
>                                    (\y -> for [0..14]
>                                           (\x -> ((ls !! y) !! x, x,y)))
>                        in filter (\(c,_,_) -> c /= ' ') sqs

================================================================================

== valid squares

Valid squares diagram looks like this:
1X     2      3\
XX             \
               \
               \
4             5\
               \
               \
               \
               \
6      7      8\

The numbers are there to get your bearing - they are in the starting
positions for each wizard and are ignored when parsing.

The Xs mark the valid squares (if one is needed on a wizard position
then you leave the wizard number out of the diagram).

> parseValidSquaresString :: String -> [(Int,Int)]
> parseValidSquaresString =
>     mapMaybe (\(c,x,y) ->
>               case c of
>                 'X' -> Just (x,y)
>                 _ -> if c `elem` ['1'..'8']
>                        then Nothing
>                        else error $ "invalid char in validsquares: " ++ [c])
>               . parseDiagram

corresponding function to read the list of valid squares from the
database hard coded to casting for now, will need to cover other stuff
as well.

> readValidSquares :: Connection -> IO [(Int, Int)]
> readValidSquares conn = do
>   v <- selectRelation conn "select x,y from current_wizard_spell_squares" []
>   return $ map (\t -> (read $ t "x", read $ t "y")) v


================================================================================

= test helpers

== valid squares

take a valid squares string and check it matches the database
it has to be the cast phase for this to succeed.

> checkValidSquares :: Connection -> String -> IO ()
> checkValidSquares conn vss = do
>     --get our piece tuple lists for the board in the database
>     --and the expected board
>     currentValidSquares <- readValidSquares conn
>     let avs' = sort currentValidSquares
>         evs' = sort $ parseValidSquaresString vss
>     assertEqual "valid squares wrong" evs' avs'

== board

The board diagram works like the valid squares, but additional needs a
key to match letters and numbers to piece lists.

An example board with the wizards in their starting positions an a
goblin owned by the first wizard next to him:

"\n\
\1G     2      3\
\               \
\               \
\               \
\4             5\
\               \
\               \
\               \
\               \
\6      7      8"
The key is:
(wizardPiecesList ++
[('G', [PieceDescription "goblin" "Buddha" []])]

wizardPiecesList is a list of the wizard pieces to avoid writing them
out each test since most tests have all eight wizards remaining at the
end

> wizardPiecesList =  [('1', [PieceDescription "wizard" "Buddha" []]),
>                      ('2', [PieceDescription "wizard" "Kong Fuzi" []]),
>                      ('3', [PieceDescription "wizard" "Laozi" []]),
>                      ('4', [PieceDescription "wizard" "Moshe" []]),
>                      ('5', [PieceDescription "wizard" "Muhammad" []]),
>                      ('6', [PieceDescription "wizard" "Shiva" []]),
>                      ('7', [PieceDescription "wizard" "Yeshua" []]),
>                      ('8', [PieceDescription "wizard" "Zarathushthra" []])]

> wizardStartPos = [(PieceDescription "wizard" "Buddha" [], 0, 0),
>                  (PieceDescription "wizard" "Kong Fuzi" [] ,7 ,0),
>                  (PieceDescription "wizard" "Laozi" [], 14, 0),
>                  (PieceDescription "wizard" "Moshe" [], 0, 4),
>                  (PieceDescription "wizard" "Muhammad" [], 14,4),
>                  (PieceDescription "wizard" "Shiva" [], 0, 9),
>                  (PieceDescription "wizard" "Yeshua" [], 7, 9),
>                  (PieceDescription "wizard" "Zarathushthra" [], 14,9)]

> wizardNames = map (\(_, [PieceDescription _ w _]) -> w) wizardPiecesList

We use these two datatypes in the key for the boards

> data PieceDescription =
>     PieceDescription String String [PieceTag]
>     deriving (Show,Eq,Ord)

> data PieceTag = PImaginary | PDead | PArmour | PUndead
>     deriving (Show,Eq,Ord)

The piece tag holds a bit of extra information about that piece to
shorten the tests and avoid having to read out lots of tables when
e.g. if a creature is imaginary or undead, etc. Will probably rethink
this when the tests are expanded in scope.

These are our type alias for the board

A key entry gives a piece list corresponding to a letter on a board
diagram

> type KeyEntry = (Char, [PieceDescription])

The full board diagram is the diagram string plus the key

> type BoardDiagram = (String, [KeyEntry])

A board description is what we read out of the database and parse the
board diagram to for comparison and loading into the database.

> type BoardDescription = [(PieceDescription,Int,Int)]

Top level fn, take the expected board description in the form of a
board string and a key, and check it against what is in the database:

> checkBoard :: Connection -> BoardDiagram -> IO ()
> checkBoard conn bd = do
>   actualBoard <- readBoard conn
>   let expectedBoard = parseBoardDiagram bd
>   assertEqual "board pieces" (sort expectedBoard) (sort actualBoard)

Like with check board, but we are only want to check the topmost piece
on each square

> checkTopPieces :: Connection -> BoardDiagram -> IO ()
> checkTopPieces conn bd = do
>   actualBoard <- readPiecesOnTop conn
>   let expectedBoard = parseBoardDiagram bd
>   assertEqual "board pieces on top" (sort expectedBoard) (sort actualBoard)



> parseBoardDiagram :: BoardDiagram -> BoardDescription
> parseBoardDiagram (diagram, key) =
>   let keyPositionList = parseDiagram diagram
>   in flip concatMap keyPositionList
>               (\(k,x,y) ->
>                    let ps = safeLookup "board diagram parse" k key
>                    in map (\p -> (p,x,y)) ps)
>

now the code to read the board from the database and get it in the same format:

> readBoard :: Connection -> IO BoardDescription
> readBoard conn =
>   selectRelation conn "select ptype,allegiance,x,y,undead\n\
>                       \from piece_details" [] >>=
>   convertBoardRel conn

The variant for only the topmost pieces:

> readPiecesOnTop :: Connection -> IO BoardDescription
> readPiecesOnTop conn =
>   selectRelation conn "select ptype,allegiance,x,y,undead\n\
>                           \from pieces_on_top_view" [] >>=
>   convertBoardRel conn
>

This is the function that takes the relation returned from the
previous two functions and turns it into a boarddescription

Adding the tags is a bit haphazard

> convertBoardRel conn v = do
>   let checkTag r t t' = if r t == "True"
>                        then Just t'
>                        else Nothing
>   let w = for v (\p ->
>                           (PieceDescription
>                            (p "ptype")
>                            (p "allegiance")
>                            (catMaybes [checkTag p "undead" PUndead]),
>                            read $ p "x",
>                            read $ p "y"))

now add tags from the wizards table, so we can check things like magic
sword

>   u <- readWizardUpgrades conn
>   return $ for w (\pd@(PieceDescription ptype allegiance tags, x, y) ->
>                 if ptype /= "wizard"
>                    then pd
>                    else (PieceDescription
>                          ptype
>                          allegiance
>                          (tags ++ (fromMaybe [] $ lookup allegiance u)),
>                          x, y))


> readWizardUpgrades :: Connection -> IO [(String, [PieceTag])]
> readWizardUpgrades conn = do
>   v <- selectRelation conn "select wizard_name, magic_armour from wizards" []
>   let checkTag r t t' = if r t == "True"
>                           then Just t'
>                           else Nothing
>   return $ for v (\vs -> (vs "wizard_name",
>                           catMaybes [checkTag vs "magic_armour" PArmour]))


== check new game relvars

run after each call to new game to check - hopefully should catch any
problems when all the tests are run since each one runs new game with
the board in the state the last test left it in

todo: add all relvars which aren't readonly to this

> checkNewGameRelvars :: Connection -> IO ()
> checkNewGameRelvars conn = do
>   checkRelvar conn "turn_number_table" [["0"]]
>   checkRelvar conn "current_wizard_table" [["Buddha"]]
>   checkRelvar conn "turn_phase_table" [["choose"]]
>   checkRelvar conn "spell_choice_hack_table" [["False"]]
> --todo: action_history
>   mapM_ (\x -> checkRelvar conn x []) ["wizard_spell_choices_mr",
>                                       "spell_parts_to_cast_table",
>                                       "cast_success_checked_table",
>                                       "cast_alignment_table",
>                                       "pieces_to_move",
>                                       "selected_piece",
>                                       "squares_left_to_walk_table"]

== some helper functions

something hacked up to check the contents of a relvar in the
database. Should create a better format for writing relvar literals in
this test file.

> checkRelvar :: Connection -> String -> [[String]] -> IO ()
> checkRelvar conn relvarName value =
>   selectRelationValues conn ("select * from " ++ relvarName) [] >>=
>   assertEqual ("relvar " ++ relvarName) value

shorthands to check data in the database

> checkSelectedPiece conn allegiance ptype = do
>   v <- selectRelation conn "select * from selected_piece" []
>   assertEqual "selected piece" (allegiance,ptype)
>               (head v "allegiance", head v "ptype")

> checkMoveSubphase conn sp = do
>   ms <- selectRelation conn "select move_phase from selected_piece" []
>   assertEqual "move subphase" sp (head ms "move_phase")

> checkNoSelectedPiece conn = do
>   v <- selectRelation conn "select * from selected_piece" []
>   assertBool "no selected piece" (null v)

================================================================================

= setup game functions

== setup board

this takes a board description and sets the board to match it used to
setup a particular game before running some tests

> setupBoard :: Connection -> BoardDiagram -> IO ()
> setupBoard conn bd = do
>   let targetBoard = parseBoardDiagram bd
>   --just assume that present wizards are in the usual starting positions
>   --fix this when needed
>   let (wizardItems, nonWizardItems) =
>          partition (\(PieceDescription t _ _, _, _) -> t == "wizard")
>          targetBoard
>       isWizPresent name = any (\(PieceDescription _ n _, _, _) ->
>                                n == name) wizardItems
>   -- remove missing wizards
>   forM_ [0..7] (\i ->
>     unless (isWizPresent $ wizardNames !! i) $
>       callSp conn "kill_wizard" [wizardNames !! i])
>   -- move present wizards
>   forM_ wizardItems (\(PieceDescription _ name _, x, y) ->
>     selectTuple conn "select x,y from pieces where ptype='wizard'\n\
>                      \and allegiance=?" [name] (\t ->
>       unless (read (t "x") == x && read (t "y") == y)
>            (runSql conn "update pieces_mr set x = ?, y = ?\n\
>                        \where ptype='wizard' and allegiance=?"
>                    [show x, show y, name])))
>   -- add extra pieces
>   forM_ nonWizardItems (\(PieceDescription ptype allegiance tags, x, y) ->
>     if allegiance == "dead"
>       then callSp conn "create_corpse"
>                   [ptype,
>                    (show x),
>                    (show y),
>                    show (PImaginary `elem` tags)]
>       else callSp conn "create_piece_internal"
>                   [ptype,
>                    allegiance,
>                    (show x),
>                    (show y),
>                    show (PImaginary `elem` tags)])
>   return ()

this overrides the next random test by name e.g. so we can test the
board after a spell has succeeded and after it has failed

> rigActionSuccess :: Connection -> String -> Bool -> IO ()
> rigActionSuccess conn override setting =
>   dbAction conn "rig_action_success" [override, show setting]

================================================================================

= database update helpers

> startNewGame :: Connection -> IO ()
> startNewGame conn = do
>   dbAction conn "reset_new_game_widget_state" []
>   dbAction conn "client_new_game_using_new_game_widget_state" []
>   checkNewGameRelvars conn


Adds the spell given to the first wizard's spell book

> addSpell conn spellName =
>   runSql conn "insert into spell_books (spell_name, wizard_name)\n\
>          \values (?, 'Buddha');" [spellName]

keep running next_phase until we get to the cast phase

> skipToPhase conn phase = do
>   unless (phase `elem` ["choose","cast","move"])
>          (error $ "unrecognised phase: " ++ phase)
>   t <- readTurnPhase conn
>   when (t /= phase) $ do
>          sendKeyPress conn "space"
>          skipToPhase conn phase

> sendKeyPress :: Connection -> String -> IO ()
> sendKeyPress conn k = dbAction conn "key_pressed" [k]

================================================================================

= database read shortcuts

> readTurnPhase :: Connection -> IO String
> readTurnPhase conn = do
>   r <- selectTupleRet conn "select turn_phase from turn_phase_table" []
>   return $ r "turn_phase"

> readCurrentWizard :: Connection -> IO String
> readCurrentWizard conn = do
>   r <- selectTupleRet conn "select current_wizard from current_wizard_table" []
>   return $ r "current_wizard"

> assertSelectedPiece :: Connection -> String -> String -> IO()
> assertSelectedPiece conn ptype allegiance =
>   selectTuple conn "select ptype, allegiance from selected_piece" []
>               (\t -> assertEqual "selected piece"
>                                  (ptype,allegiance)
>                                  (t "ptype", t "allegiance"))

> checkCurrentWizardPhase :: Connection -> String -> String -> IO()
> checkCurrentWizardPhase conn wiz phase = do
>   wiz' <- readCurrentWizard conn
>   phase' <- readTurnPhase conn
>   assertEqual "current wizard" wiz wiz'
>   assertEqual "current phase" phase' phase

================================================================================

= keyboard stuff

create wrappers around the key press stuff to make the tests easier to
understand

> lookupChooseSpellKeys = [("goblin", "m")
>                         ,("disbelieve", "Q")
>                         ,("magic_wood", "G")
>                         ,("shadow_wood", "M")
>                         ,("magic_bolt", "D")
>                         ,("vengeance", "E")
>                         ,("subversion", "R")
>                         ,("raise_dead", "V")
>                         ,("magic_armour", "3")
>                         ,("law", "O")
>                         ]

> keyChooseSpell :: String -> String
> keyChooseSpell spellName = safeLookup "get key for spell" spellName lookupChooseSpellKeys

--------------------------------------------------------------

Todo ideas for tests:

? Write tests for the more complicated logic in the sql.  e.g. some of
the constraint system, etc.

Fix up tests already written:

Add test fragments to run at every stage to check all the available
actions and all their valid arguments.

check all changes to the database after each action and also check no
other changes are made.

Fix probabilities:

write tests to run tests many times and check the probabilities

for all the actions which involve a piece moving square write a test
to cover all options e.g. test walk attack in all eight directions

make the tests hierarchical, use a better tool to run them

Make the tests more watertight: at least one test for each spell,
mirror tests for each wizard

Add variants for all actions which can be repeated: e.g. walk 3 squares
cast 4 spells.

Check any actions which don't have tests and write them.

Use a quickcheck style spec to generate more exaustive tests?

some other todos:

try to select and do stuff that isn't allowed at a particular time,
make sure nothing happens, looking to prevent the game crashing with a
database constraint fail

find some way to record a full game and replay it as a test to check
everything

run each action from each wizard, and from a monster and a wizard variations
when applicable
