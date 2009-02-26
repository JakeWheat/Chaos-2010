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
> import Control.Monad
> import Data.Maybe
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Exception

> import ChaosDB
> import Conf
> import Utils

================================================================================

= Main

Run all the tests.

> main :: IO ()
> main = time $ do
>   conf <- getConfig
>   withConn ("host=localhost dbname=" ++
>             dbName conf ++ " user=" ++ username conf ++
>             " password=" ++ password conf) $ \conn -> do

>   defaultMain [
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
>                       ,testCastVengeanceWizard conn
>                       ,testCastVengeanceMonster conn
>                       ,testCastVengeanceMonsterResisted conn
>                       ,testCastSubversion conn
>                       ,testCastSubversionResisted conn
>                       ,testCastDisbelieveReal conn
>                       ,testCastDisbelieveImaginary conn
>                       ,testCastRaiseDead conn
>                       ,testCastLaw conn
>                       ,testImaginary conn
>                       ,testGroup "upgrades" [
>                                       testCastShield conn
>                                      ,testCastArmour conn
>                                      ,testCastKnife conn
>                                      ,testCastSword conn
>                                      ,testCastBow conn
>                                      ,testCastWings conn
>                                      ,testCastShadowForm conn
>                                      ]
>                       ]
>        ,testGroup "autonomous" [
>                        testCastleDisappear conn
>                       ,testCastleStay conn
>                       ,testGetSpell conn
>                       ,testNoGetSpell conn
>                       ]
>        ,testGroup "move phase stuff" [
>           testGroup "subphases" [
>                          testWalkCancelAttackDone conn
>                         ,testCancelMotionDone conn
>                         ,testWalkNoAvailAttackDone conn
>                         ,testWAttackDone conn
>                         ,testWalk2Done conn
>                         ,testWalk1CancelDone conn
>                         ,testFlyAttackDone conn
>                         ,testFAttackDone conn
>                         ,testCancelFlyDone conn
>                         ,testFlyNoAvailAttackDone conn
>                         ,testWalkAttackRangedDone conn
>                         ,testWalkAttackCancelDone conn
>                         ,testWalkCancelRangedDone conn
>                         ,testCancelRangedDone conn
>                         ,testWalkNoAvailAttackRangedDone conn
>                         ,testAttackDone conn
>                         ,testNoAvailAttackDone conn
>                         ]
>           ,testGroup "moveMisc" [
>                           testAttackWizard conn
>                          ,testFlyAttack conn
>                          ,testMountThenMoveMount conn
>                          ,testMoveWhenMounted conn
>                          ,testDismount conn
>                          ,testMoveWhenAlreadyMounted conn
>                          ,testEnter conn
>                          ,testExit conn
>                          ,testAttackShadowForm conn
>                        ]
>           ]
>        ,testGroup "game complete" [
>                        testWizardWin conn
>                       ,testGameDraw conn
>                       ]
>                 ]

>   return ()

> tctor :: String
>       -> (Connection -> IO ())
>       -> Connection
>       -> Test.Framework.Test
> tctor l f conn = testCase l $ rollbackOnError conn $ f conn

postgresql.conf  #track_functions = none # none, pl, all

 select * from pg_stat_user_functions ;
http://www.depesz.com/index.php/2008/05/15/waiting-for-84-function-stats/

================================================================================

= Tests

== tests in sql

First run the tests from the database, these are all the database
functions whose name starts with 'check_code_'

> testDatabaseStuff :: Connection -> Test.Framework.Test
> testDatabaseStuff = tctor "testDatabaseStuff" $ \conn -> do
>   res <- selectTuplesIO conn "select object_name\n\
>                     \from module_objects\n\
>                     \where object_name ~ 'check_code_.*'\n\
>                     \and object_type='operator';" []
>                    (\t -> do
>      v <- selectValue conn ("select " ++  lk "object_name" t ++ "()") []
>      return (lk "object_name" t, read v::Bool))
>   let notpass = filter (\(_,b) -> not b) res

>   when (length notpass > 0)
>      (assertBool ("sql tests failed: " ++
>                   intercalate "\n" (map fst notpass)) False)

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

> testPiecesOnTop :: Connection -> Test.Framework.Test
> testPiecesOnTop = tctor "testPiecesOnTop" $ \conn -> do
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

> testCursorMovement :: Connection -> Test.Framework.Test
> testCursorMovement = tctor "testCursorMovement" $ \conn -> do
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
>   r <- selectTuple conn "select x,y from cursor_position" []
>   return (read $ lk "x" r, read $ lk "y" r)

move the cursor to x,y, using key presses

> moveCursorTo :: Connection -> Int -> Int -> IO ()
> moveCursorTo conn x y = do
>     --diagonals first then straight moves
>     (cx,cy) <- readCursorPosition conn
>     -- putStrLn $ "move " ++ (show (cx,cy)) ++ " to " ++ (show (x,y))
>     unless ((cx,cy) == (x,y)) $ do
>         let (dx,dy) = (x - cx, y - cy)
>             diagonalMoves = minimum [abs dx, abs dy]
>             diagonalDirection = case True of
>                                 _ | dx == 0 && dy == 0 -> "ul"
>                                   | dx < 0 && dy < 0 -> "ul"
>                                   | dx < 0 && dy > 0 -> "dl"
>                                   | dx > 0 && dy < 0 -> "ur"
>                                   | dx > 0 && dy > 0 -> "dr"
>                                   | otherwise -> error
>                                        "pattern match: something \
>                                        \wrong in moveCursorTo"
>         -- do it in two stages cos I'm not smart enough
>         sequence_ (replicate diagonalMoves
>                    (sendKeyPress conn $ cursorShorthand diagonalDirection))
>         (ncx,ncy) <- readCursorPosition conn
>         unless ((cx,cy) == (x,y)) $ do
>           let (dir,amount) = case True of
>                            _ | ncx < x -> ("r", x - ncx)
>                              | ncx > x -> ("l", ncx - x)
>                              | ncy < y -> ("d", y - ncy)
>                              | ncy > y -> ("u", ncy - y)
>                              | otherwise -> ("u", 0)
>           sequence_ (replicate amount
>                        (sendKeyPress conn $ cursorShorthand dir))


> checkCursorPosition :: Connection
>                     -> Int
>                     -> Int
>                     -> IO ()
> checkCursorPosition conn x y = do
>   cp <- readCursorPosition conn
>   assertEqual "cursor position" cp (x, y)

> goSquare :: Connection -> Int -> Int -> IO ()
> goSquare conn x y = do
>   moveCursorTo conn x y
>   sendKeyPress conn "Return"

== next phase

Just run through the choose, cast and move phases for each wizard
twice, check the turn_phase and current_wizard each time

> testNextPhase :: Connection -> Test.Framework.Test
> testNextPhase = tctor "testNextPhase" $ \conn -> do
>   startNewGame conn
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                checkCurrentWizardPhase conn (wizardNames !! i) phase
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                whenA1 (readTurnPhase conn)
>                       (=="choose")
>                       (sendKeyPress conn "Q")
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead :: Connection -> Test.Framework.Test
> testNextPhaseWizardDead = tctor "testNextPhaseWizardDead" $ \conn ->
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizardNames !! j]
>     let theseWizards = dropItemN wizardNames j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>         checkCurrentWizardPhase conn (theseWizards !! i) phase
>         whenA1 (readTurnPhase conn)
>                (=="choose")
>                (sendKeyPress conn "Q")
>         sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead :: Connection -> Test.Framework.Test
> testNextPhaseTwoWizardsDead = tctor "testNextPhaseTwoWizardsDead" $ \conn ->
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
>           whenA1 (readTurnPhase conn)
>                  (=="choose")
>                  (sendKeyPress conn "Q")
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

> testCastGoblin :: Connection -> Test.Framework.Test
> testCastGoblin = tctor "testCastGoblin" $ \conn -> do

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

> testFailCastGoblin :: Connection -> Test.Framework.Test
> testFailCastGoblin = tctor "testFailCastGoblin" $ \conn -> do
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

> testCastMagicWood :: Connection -> Test.Framework.Test
> testCastMagicWood = tctor "testCastMagicWood" $ \conn -> do
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

> testCastShadowWood :: Connection -> Test.Framework.Test
> testCastShadowWood = tctor "testCastShadowWood" $ \conn -> do
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
>   mapM_ (uncurry $ goSquare conn)
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

> testCastMagicBolt :: Connection -> Test.Framework.Test
> testCastMagicBolt = tctor "testCastMagicBolt" $ \conn -> do
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

> testCastMagicBoltResisted :: Connection -> Test.Framework.Test
> testCastMagicBoltResisted = tctor "testCastMagicBoltResisted" $ \conn -> do
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

> testCastVengeanceWizard :: Connection -> Test.Framework.Test
> testCastVengeanceWizard = tctor "testCastVengeanceWizard" $ \conn -> do
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

> testCastVengeanceMonster :: Connection -> Test.Framework.Test
> testCastVengeanceMonster = tctor "testCastVengeanceMonster" $ \conn -> do
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

> testCastVengeanceMonsterResisted :: Connection -> Test.Framework.Test
> testCastVengeanceMonsterResisted =
>     tctor "testCastVengeanceMonsterResisted" $ \conn -> do
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

> testCastSubversion :: Connection -> Test.Framework.Test
> testCastSubversion = tctor "testCastSubversion" $ \conn -> do
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

> testCastSubversionResisted :: Connection -> Test.Framework.Test
> testCastSubversionResisted = tctor "testCastSubversionResisted" $ \conn -> do
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

> testCastDisbelieveReal :: Connection -> Test.Framework.Test
> testCastDisbelieveReal = tctor "testCastDisbelieveReal" $ \conn -> do
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

> testCastDisbelieveImaginary :: Connection -> Test.Framework.Test
> testCastDisbelieveImaginary = tctor "testCastDisbelieveImaginary" $ \conn -> do
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

> testCastRaiseDead :: Connection -> Test.Framework.Test
> testCastRaiseDead = tctor "testCastRaiseDead" $ \conn -> do
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

> testCastShield :: Connection -> Test.Framework.Test
> testCastShield = tctor "testCastShield" $ \conn -> do
>   doUpgradeTest conn "magic_shield" $
>                 \t -> addStat "physical_defense" 2 t

> testCastArmour :: Connection -> Test.Framework.Test
> testCastArmour = tctor "testCastArmour" $ \conn -> do
>   doUpgradeTest conn "magic_armour" $
>                 \t -> addStat "physical_defense" 4 t

> testCastKnife :: Connection -> Test.Framework.Test
> testCastKnife = tctor "testCastKnife" $ \conn -> do
>   doUpgradeTest conn "magic_knife" $
>                 \t -> addStat "attack_strength" 2 t

> testCastSword :: Connection -> Test.Framework.Test
> testCastSword = tctor "testCastSword" $ \conn -> do
>   doUpgradeTest conn "magic_sword" $
>                 \t -> addStat "attack_strength" 4 t

> testCastBow :: Connection -> Test.Framework.Test
> testCastBow = tctor "testCastBow" $ \conn -> do
>   doUpgradeTest conn "magic_bow" $
>                 setStat "ranged_weapon_type" "projectile" .
>                 setStat "range" "6" .
>                 setStat "ranged_attack_strength" "6"

> testCastWings :: Connection -> Test.Framework.Test
> testCastWings = tctor "testCastWings" $ \conn -> do
>   doUpgradeTest conn "magic_wings" $
>                 setStat "speed" "6" .
>                 setStat "flying" "True"

> testCastShadowForm :: Connection -> Test.Framework.Test
> testCastShadowForm = tctor "testCastShadowForm" $ \conn -> do
>   doUpgradeTest conn "shadow_form" $
>                 addStat "physical_defense" 2 .
>                 addStat "agility" 2 .
>                 setStat "speed" "3"

> addStat :: String -> Int -> SqlRow -> SqlRow
> addStat att n tup = M.insert att (adds $ fromJust $ M.lookup att tup) tup
>     where
>       adds s = show $ (read s ::Int) + n

> setStat :: String -> String -> SqlRow -> SqlRow
> setStat att v tup = M.insert att v tup

> doUpgradeTest :: Connection -> String -> (SqlRow -> SqlRow)
>               -> IO ()
> doUpgradeTest conn spell attrChange = do
>   newGameReadyToCast conn spell
>   oldStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   newStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []
>   assertEqual "upgraded stats" (attrChange oldStats) newStats
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


todo:
stack upgrades - different spells, same spells e.g. magic knife
twice, magic knife then magic sword

if there is a way that a wizard can lose an upgrade that needs to be
tested too


> testCastLaw :: Connection -> Test.Framework.Test
> testCastLaw = tctor "testCastLaw" $ \conn -> do
>   newGameReadyToCast conn "law"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   align <- selectValue conn "select world_alignment\n\
>                               \from world_alignment_table" []
>   assertEqual "world alignment not law after casting law" "1" align

> testImaginary :: Connection -> Test.Framework.Test
> testImaginary = tctor "testImaginary" $ \conn -> do

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
>                                    \where x= 1 and y = 0" []
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

> newGameReadyToCast :: Connection -> String -> IO ()
> newGameReadyToCast conn spellName = do
>   startNewGame conn
>   addSpell conn spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase conn "cast"

> newGameWithBoardReadyToCast :: Connection
>                             -> String
>                             -> BoardDiagram
>                             -> IO ()
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
attack.

=== Some rules about the subphases:


A piece is unselected automatically when there are no more valid
subphases for it.

The program skips to next subphase if the selected piece can't do the
current phase e.g. if the piece cannot walk or fly then it skips the
motion subphase.

When going to the attack subphase, if the monster is not next to an
attackable enemy piece, the attack phase is skipped.

Opposite to this, when going to ranged attack subphase, the subphase
isn't skipped even if there are no enemy attackables the creature can
attack (i.e. in range and in line of sight)

Goes to the next subphase automatically if the piece has nothing left
to do in the current one e.g. has completed all its walk, or has
attacked.

A piece can attack a square in walk or fly range during the motion
subphase (going straight into attack without finishing motion).

A piece can cancel the current subphase. To go with previous rule, if
it cancels in the motion subphase, then it skips the attack phase
also, even if it is next to an attackable enemy. Therefore if the
motion subphase is cancelled, the piece skips to ranged attack if it
has one or ends its selection.

If a piece walks and can walk more that one square, it counts as
cancel if you cancel at any point before moving all the available
squares.

Pieces walk one square at a time if they can walk multiple squares,
but do a flight all in one go, i.e. for a walker you select the next
adjacent square and then repeat until all the squares to move are used
up, but for a flier you just select the end point of the flying.

A wizard mounting a monster ends its motion subphase immediately even
if it has squares left to walk.

The selected piece relvar has exactly one row during the time a piece
is selected and no rows otherwise

The walk squares relvar has exactly one row when there is a selected
piece, the selected piece: moves; walks rather than flies; and is in
the motion subphase. otherwise it has no rows

A piece can be killed whilst selected if it uses its ranged attack on
its own wizard - so need to check for this.

=== test outline

our subphase progression options for different pieces depending on
their capabilities are:

1 motion-attack-done
2 attack-done
3 motion-attack-ranged-done

We want to cover each variation of these, but we only need to cover
each transition once (e.g. motion-rangedattack). E.g. for walking, if
we test a one square walker which then attacks, for a two square
walker, we can test it can walk to squares, and then check it is ready
to attack and finish there (possibly we could skip the check it is
ready to attack part).

We want to include the variations with cancel in one of the positions
(since cancelling motion skips attack this reduces the number of
possibilities).

We also want to include walk one square for a two square walker and
cancel, walk two squares for a two square walker, and fly for each
motion variety.


So, the final list which should cover most transitions is

* piece has motion-attack potential, walks 1 square)
walk1 cancelattack done
cancelmotion done
walk1 noavailableattack done
attack-done

* piece has motion-attack potential, walks 2 squares)
walk2 done
walk1cancel done

* piece has motion-attack potential, flies)
fly attack done
attack done
cancelfly done
fly noavailableattack done

* piece has motion-attack-ranged, walks 1 square
walk1 attack ranged done
walk1 attack cancel done
walk1 cancel ranged done
cancelwalk ranged done
walk1 noavailableattack ranged done

* piece has attack only
attack done
noavailableattack done

=== tests

Because of the way the tests are written, we also test successful and
unsuccessful attack and ranged attack here, and walk and fly
variations, so we don't need to test them elsewhere.

> testWalkCancelAttackDone :: Connection -> Test.Framework.Test
> testWalkCancelAttackDone = tctor "testWalkCancelAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "goblin" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G R   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     --select goblin
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "goblin"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkMoveSubphase conn "attack"
>     sendKeyPress conn "End"
>     checkPieceDoneSelection conn "goblin" "Buddha"

> testCancelMotionDone :: Connection -> Test.Framework.Test
> testCancelMotionDone = tctor "testCancelMotionDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "goblin" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
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
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "goblin"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkPieceDoneSelection conn "goblin" "Buddha"
>     checkBoard conn ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWalkNoAvailAttackDone :: Connection -> Test.Framework.Test
> testWalkNoAvailAttackDone = tctor "testWalkNoAvailAttackDone" $
>                                    \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "goblin" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "goblin"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkPieceDoneSelection conn "goblin" "Buddha"
>     checkBoard conn ("\n\
>                   \1 G R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWAttackDone :: Connection -> Test.Framework.Test
> testWAttackDone = tctor "testWAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "goblin" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('E', [PieceDescription "goblin" "Buddha" [],
>                       PieceDescription "giant_rat" "dead" []])])
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
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "goblin"
>     checkMoveSubphase conn "motion"
>     rigActionSuccess conn "attack" True
>     goSquare conn 2 0
>     checkPieceDoneSelection conn "goblin" "Buddha"
>     checkBoard conn ("\n\
>                   \1 E    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWalk2Done :: Connection -> Test.Framework.Test
> testWalk2Done = tctor "testWalk2Done" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [PieceDescription "bear" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1B   R 2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "bear"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkSelectedPiece conn "Buddha" "bear"
>     checkMoveSubphase conn "motion"
>     checkBoard conn ("\n\
>                   \1 B  R 2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 3 0
>     checkPieceDoneSelection conn "bear" "Buddha"
>     checkBoard conn ("\n\
>                   \1  B R 2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWalk1CancelDone :: Connection -> Test.Framework.Test
> testWalk1CancelDone = tctor "testWalk1CancelDone" $
>                              \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [PieceDescription "bear" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1B R   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "bear"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkSelectedPiece conn "Buddha" "bear"
>     checkMoveSubphase conn "motion"
>     checkBoard conn ("\n\
>                   \1 BR   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     sendKeyPress conn "End"
>     checkPieceDoneSelection conn "bear" "Buddha"

> testFlyAttackDone :: Connection -> Test.Framework.Test
> testFlyAttackDone = tctor "testFlyAttackDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "eagle" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('H', [PieceDescription "eagle" "Buddha" [],
>                       PieceDescription "giant_rat" "dead" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "eagle"
>     checkMoveSubphase conn "motion"
>     goSquare conn 3 0
>     checkBoard conn ("\n\
>                   \1  GR  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkMoveSubphase conn "attack"
>     rigActionSuccess conn "attack" True
>     goSquare conn 4 0
>     checkPieceDoneSelection conn "eagle" "Buddha"
>     checkBoard conn ("\n\
>                   \1   H  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testFAttackDone :: Connection -> Test.Framework.Test
> testFAttackDone = tctor "testFAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "eagle" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "eagle"
>     checkMoveSubphase conn "motion"
>     rigActionSuccess conn "attack" False
>     goSquare conn 4 0
>     checkPieceDoneSelection conn "eagle" "Buddha"
>     checkBoard conn ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testCancelFlyDone :: Connection -> Test.Framework.Test
> testCancelFlyDone = tctor "testCancelFlyDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "eagle" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
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
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "eagle"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkPieceDoneSelection conn "eagle" "Buddha"
>     checkBoard conn ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testFlyNoAvailAttackDone :: Connection -> Test.Framework.Test
> testFlyNoAvailAttackDone = tctor "testFlyNoAvailAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "eagle" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G    R2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "eagle"
>     checkMoveSubphase conn "motion"
>     goSquare conn 3 0
>     checkPieceDoneSelection conn "eagle" "Buddha"
>     checkBoard conn ("\n\
>                   \1  G  R2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)


* piece has motion-attack-ranged, walks 1 square

> testWalkAttackRangedDone :: Connection -> Test.Framework.Test
> testWalkAttackRangedDone = tctor "testWalkAttackRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "elf" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('H', [PieceDescription "elf" "Buddha" [],
>                       PieceDescription "giant_rat" "dead" []]),
>                ('r', [PieceDescription "giant_rat" "dead" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "elf"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkMoveSubphase conn "attack"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "attack" True
>     goSquare conn 3 0
>     checkMoveSubphase conn "ranged-attack"
>     checkBoard conn ("\n\
>                   \1  H   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "ranged_attack" True
>     goSquare conn 3 1
>     checkBoard conn ("\n\
>                   \1  H   2      3\n\
>                   \   r           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkPieceDoneSelection conn "elf" "Buddha"

> testWalkAttackCancelDone :: Connection -> Test.Framework.Test
> testWalkAttackCancelDone = tctor "testWalkAttackCancelDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "elf" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('H', [PieceDescription "elf" "Buddha" [],
>                       PieceDescription "giant_rat" "dead" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "elf"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkMoveSubphase conn "attack"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "attack" True
>     goSquare conn 3 0
>     checkMoveSubphase conn "ranged-attack"
>     checkBoard conn ("\n\
>                   \1  H   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     sendKeyPress conn "End"
>     checkPieceDoneSelection conn "elf" "Buddha"
>     checkBoard conn ("\n\
>                   \1  H   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWalkCancelRangedDone :: Connection -> Test.Framework.Test
> testWalkCancelRangedDone = tctor "testWalkCancelRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "elf" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "elf"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkMoveSubphase conn "attack"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "ranged-attack"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "ranged_attack" False
>     goSquare conn 3 1
>     checkPieceDoneSelection conn "elf" "Buddha"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testCancelRangedDone :: Connection -> Test.Framework.Test
> testCancelRangedDone = tctor "testCancelRangedDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "elf" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('r', [PieceDescription "giant_rat" "dead" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 2 0
>     checkSelectedPiece conn "Buddha" "elf"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     checkMoveSubphase conn "ranged-attack"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "ranged_attack" True
>     goSquare conn 3 1
>     checkPieceDoneSelection conn "elf" "Buddha"
>     checkBoard conn ("\n\
>                   \1 GR   2      3\n\
>                   \   r           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testWalkNoAvailAttackRangedDone :: Connection -> Test.Framework.Test
> testWalkNoAvailAttackRangedDone =
>   tctor "testWalkNoAvailAttackRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [PieceDescription "elf" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "elf"
>     checkMoveSubphase conn "motion"
>     goSquare conn 2 0
>     checkBoard conn ("\n\
>                   \1 G R  2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkMoveSubphase conn "ranged-attack"
>     checkBoard conn ("\n\
>                   \1 G R  2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "ranged_attack" False
>     goSquare conn 3 1
>     checkPieceDoneSelection conn "elf" "Buddha"
>     checkBoard conn ("\n\
>                   \1 G R  2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)


* piece has attack only

> testAttackDone :: Connection -> Test.Framework.Test
> testAttackDone = tctor "testAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [PieceDescription "shadow_tree" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []]),
>                ('r', [PieceDescription "giant_rat" "dead" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1WR    2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkSelectedPiece conn "Buddha" "shadow_tree"
>     checkMoveSubphase conn "attack"
>     rigActionSuccess conn "attack" True
>     goSquare conn 2 0
>     checkBoard conn ("\n\
>                   \1Wr    2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkPieceDoneSelection conn "shadow_tree" "Buddha"

> testNoAvailAttackDone :: Connection -> Test.Framework.Test
> testNoAvailAttackDone = tctor "testNoAvailAttackDone" $
>                                \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [PieceDescription "shadow_tree" "Buddha" []]),
>                ('R', [PieceDescription "giant_rat" "Kong Fuzi" []])])
>     startNewGameReadyToMove conn ("\n\
>                   \1W     2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     goSquare conn 1 0
>     checkPieceDoneSelection conn "shadow_tree" "Buddha"



== other move action tests

> testAttackWizard :: Connection -> Test.Framework.Test
> testAttackWizard = tctor "testAttackWizard" $
>                           \conn -> do
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

> testFlyAttack :: Connection -> Test.Framework.Test
> testFlyAttack = tctor "testFlyAttack" $ \conn -> do
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

> testMountThenMoveMount :: Connection -> Test.Framework.Test
> testMountThenMoveMount = tctor "testMountThenMoveMount" $
>                              \conn -> do
>   let pl = wizardPiecesList ++ [('P', [PieceDescription "pegasus" "Buddha" []]),
>             ('M', [PieceDescription "wizard" "Buddha" [],
>                    PieceDescription "pegasus" "Buddha" []])]
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
>                   \6      7      8",pl)
>   goSquare conn 0 0
>   assertSelectedPiece conn "wizard" "Buddha"
>   goSquare conn 1 0
>   sendKeyPress conn "Return"
>   assertSelectedPiece conn "pegasus" "Buddha"
>   goSquare conn 2 2
>   checkBoard conn ("\n\
>                   \       2      3\n\
>                   \               \n\
>                   \  M            \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)


Test that the first time when try to select a square with a mounted
wizard we get the wizard, if we cancel then select again on that
square, we should get the monster the wizards is mounted on. To reduce
the number of tests we also test moving the monster whilst the wizard
is mounted here.

> testMoveWhenMounted :: Connection -> Test.Framework.Test
> testMoveWhenMounted = tctor "testMoveWhenMounted" $
>                              \conn -> do
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

> testDismount :: Connection -> Test.Framework.Test
> testDismount = tctor "testDismount" $ \conn -> do
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

> testMoveWhenAlreadyMounted :: Connection -> Test.Framework.Test
> testMoveWhenAlreadyMounted = tctor "testMoveWhenAlreadyMounted" $
>                                     \conn -> do
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

> testExit :: Connection -> Test.Framework.Test
> testExit = tctor "testExit" $ \conn -> do
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

> testEnter :: Connection -> Test.Framework.Test
> testEnter = tctor "testEnter" $ \conn -> do
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

> testAttackShadowForm :: Connection -> Test.Framework.Test
> testAttackShadowForm = tctor "testAttackShadowForm" $ \conn -> do
>   newGameWithBoardReadyToCast conn "shadow_form"
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
>   oldStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []
>   sendKeyPress conn "Return"
>   skipToPhase conn "move"
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" False
>   goSquare conn 1 0
>   newStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []
>   assertEqual "attacking loses shadow form" oldStats newStats


testAttackUndeadOnUndead

testAttackNonUndeadOnUndead

testMagicWeaponOnUndead (success, no corpse)


== misc todo

engaged stuff: engaged at start of move, and becoming engaged part way
through walk for multiwalkers

second monster dying on a square - the first corpse should disappear permanently

check moving 2 diagonal squares uses up three squares to move

check mount ends motion subphase for wizard



check when moving a mounted wizard, that the corpse on that square stays put

trying to walk/fly to occupied squares

cobra attack dragon

test select one creature and move, then select a second creature and move


test monsters in blob cannot be selected, but can be
moved if free'd that turn
 - so check if in selected list, try to select, check failure, then
  kill blob and select and move

shadow form attack and moving, shadow form and magic wings at same time

check attack when has mount moves wizard
check attack,nter,mount from mount


> startNewGameReadyToMove :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToMove conn board = do
>   startNewGame conn
>   setupBoard conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase conn "move"

> startNewGameReadyToAuto :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToAuto conn board = do
>   startNewGame conn
>   setupBoard conn board
>   sendKeyPress conn $ keyChooseSpell "disbelieve"
>   skipToPhase conn "cast"


================================================================================

= autonomous action tests

fire, blob spreading

test single square spread success, fail

spreading algorithm
run through each square on the board which is next to a blob or is a blob
if not a blob, then the chance of spreading is 20% x adjacent blobs + 10% x diagonal blobs limited to 80%.
if a blob, then the chance of disappearing is 20% if one adjacent, 40% if 4 or more, 10% otherwise.

TODO: limit the total population of a blob/fire colony? - the chance of disappearing goes up when there are more blobs and the chance of spreading goes down?

-- > testBlobSpread :: Connection -> Test.Framework.Test
-- > testBlobSpread = tctor "testBlobSpread" $ \conn -> do

-- setup the game, get to cast phase with the first wizard having
-- chosen goblin

-- >   startNewGame conn
-- >   addSpell conn "goblin"
-- >   sendKeyPress conn $ keyChooseSpell "goblin"
-- >   --get the next wizard to select disbelieve so we can check the
-- >   --auto next phase works
-- >   sendKeyPress conn "space"
-- >   sendKeyPress conn $ keyChooseSpell "disbelieve"
-- >   skipToPhase conn "cast"


test single square disappear

test spreading onto square with pieces

castles disappearing

> testCastleDisappear :: Connection -> Test.Framework.Test
> testCastleDisappear = tctor "testCastleDisappear" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [PieceDescription "wizard" "Buddha" [],
>                    PieceDescription "dark_citadel" "Buddha" []]),
>             ('C', [PieceDescription "dark_citadel" "Buddha" []])]
>   startNewGameReadyToAuto conn ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "disappear" True
>   sendKeyPress conn "space"
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
>                   \6      7      8",pl)

> testCastleStay :: Connection -> Test.Framework.Test
> testCastleStay = tctor "testCastleStay" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [PieceDescription "wizard" "Buddha" [],
>                    PieceDescription "dark_citadel" "Buddha" []]),
>             ('C', [PieceDescription "dark_citadel" "Buddha" []])]
>   startNewGameReadyToAuto conn ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "disappear" False
>   sendKeyPress conn "space"
>   checkBoard conn ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> selectInt :: Connection -> String -> [String] -> IO Int
> selectInt conn q a = do
>     x <- selectValue conn q a
>     return $ (read x ::Int)

> testGetSpell :: Connection -> Test.Framework.Test
> testGetSpell = tctor "testGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [PieceDescription "wizard" "Buddha" [],
>                    PieceDescription "magic_tree" "Buddha" []])]
>   numSpells <- selectInt conn
>          "select count(*) from spell_books where wizard_name='Buddha'" []
>   startNewGameReadyToAuto conn ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "bonus" True
>   sendKeyPress conn "space"
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
>                   \6      7      8",pl)
>   newNumSpells <- selectInt conn
>          "select count(*) from spell_books where wizard_name='Buddha'" []
>   assertEqual "got a new spell" (numSpells + 1) newNumSpells

> testNoGetSpell :: Connection -> Test.Framework.Test
> testNoGetSpell = tctor "testNoGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [PieceDescription "wizard" "Buddha" [],
>                    PieceDescription "magic_tree" "Buddha" []])]
>   startNewGameReadyToAuto conn ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "bonus" False
>   sendKeyPress conn "space"
>   checkBoard conn ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

================================================================================

= winning/drawing

Win is tested when next phase is called following the original
chaos. We check that the win has been detected by seeing the win
history item in the history table, and by checking the valid activate
and target action views are empty.

> testWizardWin :: Connection -> Test.Framework.Test
> testWizardWin = tctor "testWizardWin" $ \conn -> do
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
>   assertRelvarValue "game won history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game won';" []
>                     (1::Int)
>   assertRelvarValue "now valid target actions"
>                     conn "select count(1)\n\
>                          \from client_valid_target_actions;" []
>                     (0::Int)
>   assertRelvarValue "now valid activate actions"
>                     conn "select count(1)\n\
>                          \from client_valid_activate_actions;" []
>                     (0::Int)

> assertRelvarValue :: (Read a,Eq a,Show a) =>
>                      String -> Connection -> String -> [String] -> a -> IO ()
> assertRelvarValue m conn q args val = do
>   v <- selectValue conn q args
>   assertEqual m val (read v)

Draw works similarly to win, except it is detected the instant there
are no wizards left.

> testGameDraw :: Connection -> Test.Framework.Test
> testGameDraw = tctor "testGameDraw" $ \conn -> do
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
>   assertRelvarValue "game drawn history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game drawn';" []
>                     (1::Int)
>   assertRelvarValue "now valid target actions"
>                     conn "select count(1)\n\
>                          \from client_valid_target_actions;" []
>                     (0::Int)
>   assertRelvarValue "now valid activate actions"
>                     conn "select count(1)\n\
>                          \from client_valid_activate_actions;" []
>                     (0::Int)


================================================================================

= Diagrams

In the testing, we frequently use a square list which is either
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
>     in case True of
>          _ | null ls' -> error "board diagram empty"
>            | head ls' /= "" ->
>                error $ "first line in diagram should be empty string, got " ++
>                        head ls'
>            | otherwise ->
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
> readValidSquares conn =
>   selectTuplesC conn "select x,y from current_wizard_spell_squares" []
>     (\t -> (read $ lk "x" t, read $ lk "y" t))


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

> wizardPiecesList :: [(Char, [PieceDescription])]
> wizardPiecesList =  [('1', [PieceDescription "wizard" "Buddha" []]),
>                      ('2', [PieceDescription "wizard" "Kong Fuzi" []]),
>                      ('3', [PieceDescription "wizard" "Laozi" []]),
>                      ('4', [PieceDescription "wizard" "Moshe" []]),
>                      ('5', [PieceDescription "wizard" "Muhammad" []]),
>                      ('6', [PieceDescription "wizard" "Shiva" []]),
>                      ('7', [PieceDescription "wizard" "Yeshua" []]),
>                      ('8', [PieceDescription "wizard" "Zarathushthra" []])]

> wizardStartPos :: [(PieceDescription, Int, Int)]
> wizardStartPos = [(PieceDescription "wizard" "Buddha" [], 0, 0),
>                  (PieceDescription "wizard" "Kong Fuzi" [] ,7 ,0),
>                  (PieceDescription "wizard" "Laozi" [], 14, 0),
>                  (PieceDescription "wizard" "Moshe" [], 0, 4),
>                  (PieceDescription "wizard" "Muhammad" [], 14,4),
>                  (PieceDescription "wizard" "Shiva" [], 0, 9),
>                  (PieceDescription "wizard" "Yeshua" [], 7, 9),
>                  (PieceDescription "wizard" "Zarathushthra" [], 14,9)]

> wizardNames :: [String]
> wizardNames = map (\(_, [PieceDescription _ w _]) -> w) wizardPiecesList

We use these two datatypes in the key for the boards

> data PieceDescription =
>     PieceDescription String String [PieceTag]
>     deriving (Show,Eq,Ord)

> data PieceTag = PImaginary | PUndead
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
>   selectTuples conn "select ptype,allegiance,x,y,undead\n\
>                       \from piece_details" [] >>=
>     convertBoardRel conn

The variant for only the topmost pieces:

> readPiecesOnTop :: Connection -> IO BoardDescription
> readPiecesOnTop conn =
>   selectTuples conn "select ptype,allegiance,x,y,undead\n\
>                           \from pieces_on_top_view" [] >>=
>   convertBoardRel conn
>

This is the function that takes the relation returned from the
previous two functions and turns it into a boarddescription

Adding the tags is a bit haphazard

> convertBoardRel :: Connection -> [SqlRow] -> IO BoardDescription
> convertBoardRel _ v = do
>   let checkTag r t t' = if lk t r == "True"
>                        then Just t'
>                        else Nothing
>   let w = for v (\p ->
>                           (PieceDescription
>                            (lk "ptype" p)
>                            (lk "allegiance" p)
>                            (catMaybes [checkTag p "undead" PUndead]),
>                            read $ lk "x" p,
>                            read $ lk "y" p))
>   return w

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
 >   v <- selectTuples conn "select wizard_name, magic_armour from wizards" []
 >   let checkTag r t t' = if lk t r == "True"
 >                           then Just t'
 >                           else Nothing
 >   return $ for v (\vs -> (lk "wizard_name" vs,
 >                           catMaybes [checkTag vs "magic_armour" PArmour]))


== check new game relvars

run after each call to new game to check - hopefully should catch any
problems when all the tests are run since each one runs new game with
the board in the state the last test left it in

todo: add all relvars which aren't readonly to this

> checkNewGameRelvars :: Connection -> IO ()
> checkNewGameRelvars conn = do
>   checkRelvar conn "turn_number_table" ["0"]
>   checkRelvar conn "current_wizard_table" ["Buddha"]
>   checkRelvar conn "turn_phase_table" ["choose"]
>   checkRelvar conn "spell_choice_hack_table" ["False"]
> --todo: action_history
>   mapM_ (\x -> checkRelvar conn x []) ["wizard_spell_choices_mr"
>                                       ,"spell_parts_to_cast_table"
>                                       ,"cast_success_checked_table"
>                                       ,"cast_alignment_table"
>                                       ,"pieces_to_move"
>                                       ,"selected_piece"
>                                       ,"remaining_walk_table"]

== some helper functions

something hacked up to check the contents of a relvar in the
database. Should create a better format for writing relvar literals in
this test file.

> checkRelvar :: Connection -> String -> [String] -> IO ()
> checkRelvar conn relvarName value =
>   selectTuplesC conn ("select * from " ++ relvarName) []
>                      (\r -> head $ map snd $ M.toList r) >>=
>     assertEqual ("relvar " ++ relvarName) value

shorthands to check data in the database

> checkSelectedPiece :: Connection -> [Char] -> [Char] -> IO ()
> checkSelectedPiece conn allegiance ptype = do
>   v <- selectTuple conn "select * from selected_piece" []
>   assertEqual "selected piece" (allegiance,ptype)
>               (lk "allegiance" v, lk "ptype" v)

> checkMoveSubphase :: Connection -> [Char] -> IO ()
> checkMoveSubphase conn sp = do
>   ms <- selectTuple conn "select move_phase from selected_piece" []
>   assertEqual "move subphase" sp (lk "move_phase" ms)

> checkNoSelectedPiece :: Connection -> IO ()
> checkNoSelectedPiece conn = do
>   v <- selectTupleIf conn "select * from selected_piece" []
>   assertBool ("there should be no selected piece, got " ++
>               let r =  fromJust v
>               in lk "ptype" r ++ " - " ++ lk "allegiance" r ++
>                  " " ++ lk "move_phase" r)
>              (isNothing v)

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
>   forM_ wizardItems (\(PieceDescription _ name _, x, y) -> do
>     t <- selectTuple conn "select x,y from pieces where ptype='wizard'\n\
>                      \and allegiance=?" [name]
>     unless (read (lk "x" t) == x && read (lk "y" t) == y)
>            (runSql conn "update pieces set x = ?, y = ?\n\
>                        \where ptype='wizard' and allegiance=?"
>                    [show x, show y, name]))
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

> addSpell :: Connection -> String -> IO ()
> addSpell conn spellName =
>   runSql conn "insert into spell_books (spell_name, wizard_name)\n\
>          \values (?, 'Buddha');" [spellName]

keep running next_phase until we get to the cast phase

> skipToPhase :: Connection -> [Char] -> IO ()
> skipToPhase conn phase = do
>   unless (phase `elem` ["choose","cast","move"])
>          (error $ "unrecognised phase: " ++ phase)
>   whenA1 (readTurnPhase conn)
>          (/= phase) $ do
>          sendKeyPress conn "space"
>          skipToPhase conn phase

> sendKeyPress :: Connection -> String -> IO ()
> sendKeyPress conn k = dbAction conn "key_pressed" [k]

> rollbackOnError :: Connection -> IO c -> IO c
> rollbackOnError conn =
>     bracketOnError (return())
>                    (const $ runSql conn "rollback" []) . const

================================================================================

= database read shortcuts

> readTurnPhase :: Connection -> IO String
> readTurnPhase conn =
>   selectValue conn "select turn_phase from turn_phase_table" []

> readCurrentWizard :: Connection -> IO String
> readCurrentWizard conn =
>   selectValue conn
>          "select current_wizard from current_wizard_table" []

> assertSelectedPiece :: Connection -> String -> String -> IO()
> assertSelectedPiece conn ptype allegiance = do
>   t <- selectTuple conn "select ptype, allegiance from selected_piece" []
>   assertEqual "selected piece"
>               (ptype,allegiance)
>               (lk "ptype" t, lk "allegiance" t)

> checkCurrentWizardPhase :: Connection -> String -> String -> IO()
> checkCurrentWizardPhase conn wiz phase = do
>   wiz' <- readCurrentWizard conn
>   phase' <- readTurnPhase conn
>   assertEqual "current wizard" wiz wiz'
>   assertEqual "current phase" phase' phase

> checkPieceDoneSelection :: Connection -> String -> String -> IO ()
> checkPieceDoneSelection conn ptype allegiance = do
>     checkNoSelectedPiece conn
>     v <- selectTuples conn
>                       "select * from pieces_to_move\n\
>                       \where ptype=? and\n\
>                       \  allegiance=?" [ptype,allegiance]
>     assertBool "piece not in ptm" (null v)

================================================================================

= keyboard stuff

create wrappers around the key press stuff to make the tests easier to
understand

> lookupChooseSpellKeys :: [([Char], [Char])]
> lookupChooseSpellKeys = [("goblin", "m")
>                         ,("disbelieve", "Q")
>                         ,("magic_wood", "G")
>                         ,("shadow_wood", "M")
>                         ,("magic_bolt", "D")
>                         ,("vengeance", "E")
>                         ,("subversion", "R")
>                         ,("raise_dead", "V")
>                         ,("magic_knife", "1")
>                         ,("magic_shield", "2")
>                         ,("magic_armour", "3")
>                         ,("magic_bow", "4")
>                         ,("magic_sword", "5")
>                         ,("shadow_form", "6")
>                         ,("magic_wings", "7")
>                         ,("law", "O")
>                         ]


> keyChooseSpell :: String -> String
> keyChooseSpell spellName = safeLookup
>                              "get key for spell" spellName
>                              lookupChooseSpellKeys

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

Use a quickcheck style spec to generate more exhaustive tests?

some other todos:

try to select and do stuff that isn't allowed at a particular time,
make sure nothing happens, looking to prevent the game crashing with a
database constraint fail

find some way to record a full game and replay it as a test to check
everything

run each action from each wizard, and from a monster and a wizard variations
when applicable
