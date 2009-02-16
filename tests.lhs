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

Todo for tests:

finish off move phase action tests

finish the phase change tests

test moving to next phase automatically

? Write tests for the more complicated logic in the sql.  e.g. pieces
on top, line of sight, some of the constraint system, etc.

Fix up tests already written:

Add test fragments to run at every stage to check all the available
actions and all their valid arguments.

check all changes to the database after each action and check no other
changes are made also.

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

win game test

try to select and do stuff that isn't allowed at a particular time,
make sure nothing happens, looking to prevent the game crashing with a
database constraint fail

find some way to record a full game and replay it as a test to check
everything

run each action from each wizard, and from a monster and a wizard variations
when applicable


main list of relvars to check:

law/chaos
wizards
spellbooks
pieces
turnnumber
currentwizard
turnphase
wizard spell choices
wizard spell choices imaginary
spell choice hack
spell parts to cast
cast success checked
cast alignment
pieces to move
selected piece

squares left to walk



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

================================================================================

= Main

Run all the tests.

> main = time $ do
>   conf <- getConfig
>   withConn ("host=localhost dbname=" ++
>             dbName conf ++ " user=" ++ username conf ++
>             " password=" ++ password conf) (\conn -> do
>   runTestTT $ TestList [

>         testDatabaseStuff conn,
>         testCursorMovement conn,
>         testNextPhase conn,
>         testNextPhaseWizardDead conn,
>         testPiecesOnTop conn,
>         testMoveSubphases1 conn,
>         testMoveSubphases2 conn,
>         testMoveSubphases3 conn,
>         testMoveSubphases4 conn,
>         testCastGoblin conn,
>         testFailCastGoblin conn,
>         testCastMagicWood conn,
>         testCastShadowWood conn,
>         testCastMagicBolt conn,
>         testCastMagicBoltResisted conn,
>         testCastVegeanceWizard conn,
>         testCastVegeanceMonster conn,
>         testCastVegeanceMonsterResisted conn,
>         testCastSubversion conn,
>         testCastSubversionResisted conn,
>         testCastDisbelieveReal conn,
>         testCastDisbelieveImaginary conn,
>         testCastRaiseDead conn,
>         testCastArmour conn,
>         testCastLaw conn,
>         testWalkOneSquare conn,
>         testWalkTwoSquares conn,
>         testFlyOverPieces conn,
>         testAttackMonster conn,
>         testAttackMonsterResisted conn,
>         testAttackWizard conn,
>         testFlyAttack conn,
>         testFlyThenAttack conn,
>         testRangedAttack conn,
>         testRangedAttackResisted conn,
>         testImaginary conn,
>         testShadowWoodAttack conn,
>         testMount conn

>         ])

================================================================================

= Tests

== tests in sql

First run the tests from the database, these are all the database
functions whose name starts with 'check_code_'

> testDatabaseStuff conn = TestLabel "testDatabaseStuff" $ TestCase $ do
>   res <- newIORef ([]::[(String,Bool)])
>   selectTuples conn "select object_name\n\
>                     \from module_objects\n\
>                     \where object_name ~ 'check_code_.*'\n\
>                     \and object_type='operator';"
>                    (\t ->
>      selectValue conn ("select " ++  t "object_name" ++ "()")
>                     (\v -> do
>                        r1 <- readIORef res
>                        writeIORef res $ r1 ++
>                          [(t "object_name", read v::Bool)]))
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

> testPiecesOnTop conn = TestLabel "testPiecesOnTop" $ TestCase $ do
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

> testCursorMovement conn = TestLabel "testCursorMovement" $ TestCase $ do
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

> cursorShorthand m = fromJust $ lookup m
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
>   r <- selectRelation conn "select x,y from cursor_position"
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


== next phase

Just run through the choose, cast and move phases for each wizard
twice, check the turn_phase and current_wizard each time

> testNextPhase conn = TestLabel "testNextPhase" $ TestCase $ do
>   startNewGame conn
>   let wizards = map (\(_, [PieceDescription _ w _]) -> w) wizardPiecesList
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                tp <- readTurnPhase conn
>                cw <- readCurrentWizard conn
>                assertEqual "tp" phase tp
>                assertEqual "cw" (wizards !! i) cw
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                when (tp == "choose")
>                     (sendKeyPress conn "Q")
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> dropItemN :: [a] -> Int -> [a]
> dropItemN [] i = []
> dropItemN (x:xs) i = if i == 0
>                        then xs
>                        else x: dropItemN xs (i - 1)

> testNextPhaseWizardDead conn = TestLabel "testNextPhaseWizardDead" $ TestCase $ do
>   let wizards = map (\(_, [PieceDescription _ w _]) -> w) wizardPiecesList
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizards !! j]
>     let theseWizards = dropItemN wizards j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>          tp <- readTurnPhase conn
>          cw <- readCurrentWizard conn
>          assertEqual "tp" phase tp
>          assertEqual "cw" (theseWizards !! i) cw
>          --so we don't skip the cast phase, make sure
>          -- each wizard has a spell chosen, use disbelieve
>          --cos wizards always have this spell available
>          when (tp == "choose")
>               (sendKeyPress conn "Q")
>          --when (i == 6) $ error "hhalt"
>          sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead conn = TestLabel "testNextPhaseTwoWizardsDead" $ TestCase $ do

first two, last two, middle two

>   startNewGame conn
>   let wizards = map (\(_, [PieceDescription _ w _]) -> w) wizardPiecesList
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                tp <- readTurnPhase conn
>                cw <- readCurrentWizard conn
>                assertEqual "tp" phase tp
>                assertEqual "cw" (wizards !! i) cw
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                when (tp == "choose")
>                     (sendKeyPress conn "Q")
>                sendKeyPress conn "space"))


check wizards dying during move when it is their turn - this can
happen if you shoot your own wizard with a ranged weapon from a
monster, the game should cope with it

================================================================================

= spell cast tests

> testCastGoblin conn = TestLabel "testCastGoblin" $ TestCase $ do

setup the game, get to cast phase with the first wizard having
chosen goblin

>   startNewGame conn
>   addSpell conn "goblin"
>   sendKeyPress conn "m"
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

>   moveCursorTo conn 0 0
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Right"
>   sendKeyPress conn "Return"
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

> testFailCastGoblin conn = TestLabel "testFailCastGoblin" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "goblin"
>   sendKeyPress conn "m"
>   skipToPhase conn "cast"
>   moveCursorTo conn 0 0
>   rigActionSuccess conn "cast" False
>   sendKeyPress conn "Right"

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
>                   wizardPiecesList)

> testCastMagicWood conn = TestLabel "testCastMagicWood" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "magic_wood"
>   sendKeyPress conn "G"
>   skipToPhase conn "cast"
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

> testCastShadowWood conn = TestLabel "testCastShadowWood" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "shadow_wood"
>   sendKeyPress conn "M"
>   skipToPhase conn "cast"
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
>   mapM_ (\(x,y) -> do
>                    moveCursorTo conn x y
>                    sendKeyPress conn "Return")
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

> testCastMagicBolt conn = TestLabel "testCastMagicBolt" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "magic_bolt"
>   sendKeyPress conn "D"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 0 4
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   sendKeyPress conn "Return"
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

> testCastMagicBoltResisted conn =
>     TestLabel "testCastMagicBoltResisted" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "magic_bolt"
>   sendKeyPress conn "D"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 0 4
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
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
>                   wizardPiecesList)


> testCastVegeanceWizard conn =
>     TestLabel "testCastVegeanceWizard" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "vengeance"
>   sendKeyPress conn "E"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 7 0
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
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
>                   wizardPiecesList)

> testCastVegeanceMonster conn =
>     TestLabel "testCastVegeanceMonster" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "vengeance"
>   sendKeyPress conn "E"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   sendKeyPress conn "Return"
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
>     TestLabel "testCastVegeanceMonsterResisted" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "vengeance"
>   sendKeyPress conn "E"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
>   sendKeyPress conn "Return"
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


> testCastSubversion conn = TestLabel "testCastSubversion" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "subversion"
>   sendKeyPress conn "R"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" False
>   sendKeyPress conn "Return"
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


> testCastSubversionResisted conn =
>     TestLabel "testCastSubversionResisted" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "subversion"
>   sendKeyPress conn "R"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "cast" True
>   rigActionSuccess conn "resist" True
>   sendKeyPress conn "Return"
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


> testCastDisbelieveReal conn =
>     TestLabel "testCastDisbelieveReal" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "disbelieve"
>   sendKeyPress conn "Q"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
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

> testCastDisbelieveImaginary conn =
>     TestLabel "testCastDisbelieveImaginary" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "disbelieve"
>   sendKeyPress conn "Q"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
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
>                   wizardPiecesList)
>

> testCastRaiseDead conn = TestLabel "testCastRaiseDead" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   addSpell conn "raise_dead"
>   sendKeyPress conn "V"
>   skipToPhase conn "cast"
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
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
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


> testCastArmour conn = TestLabel "testCastArmour" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "magic_armour"
>   sendKeyPress conn "3"
>   skipToPhase conn "cast"
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


> testCastLaw conn = TestLabel "testCastLaw" $ TestCase $ do
>   startNewGame conn
>   addSpell conn "law"
>   sendKeyPress conn "O"
>   skipToPhase conn "cast"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   dbAlign <- selectRelation conn "select world_alignment\n\
>                                  \from world_alignment_table"
>   assertEqual "world alignment not law after casting law"
>              1 (read $ head dbAlign "world_alignment")


> testImaginary conn = TestLabel "testImaginary" $ TestCase $ do

>   let setStuffUp1 = do
>                     startNewGame conn
>                     addSpell conn "goblin"
>                     sendKeyPress conn "m"
>   let setStuffUp2 = do
>                     skipToPhase conn "cast"
>                     moveCursorTo conn 0 0
>                     rigActionSuccess conn "cast" True
>                     sendKeyPress conn "Right"
>   let sv = selectValue conn "select imaginary from monster_pieces\n\
>                    \natural inner join pieces\n\
>                    \where x= 1 and y = 0"
>   setStuffUp1
>   setStuffUp2
>   sendKeyPress conn "Return"
>   sv (\v -> assertBool "default real for monsters" $ not v)

>   setStuffUp1
>   sendKeyPress conn "n"
>   setStuffUp2
>   sendKeyPress conn "Return"
>   sv (\v -> assertBool "cast real monster" $ not v)

>   setStuffUp1
>   sendKeyPress conn "y"
>   setStuffUp2
>   sendKeyPress conn "Return"
>   sv (\v -> assertBool "cast imaginary monster" v)

>   setStuffUp1
>   sendKeyPress conn "y"
>   sendKeyPress conn "n"
>   setStuffUp2
>   sendKeyPress conn "Return"
>   sv (\v -> assertBool "dither then cast real monster" $ not v)

>   setStuffUp1
>   sendKeyPress conn "n"
>   sendKeyPress conn "y"
>   setStuffUp2
>   sendKeyPress conn "Return"
>   sv (\v -> assertBool "dither then cast imaginary monster" v)
>

what are the side effects to check for when casting spells:
spell direct effect
spell book looses spell
history added
world alignment changed
turn phase changed
check it's the next wizards turn

================================================================================

= move actions

== move subphases

check that the game progresses through the subphases correctly
just do one for a dragon which cancels move, cancels ranged attack
and cancels attack for now

TODO:
check variations where each action is used or cancelled
substitute walk for fly
1 walk-done
2 walk-attack-done
3 walk-ranged-done
4 walk-attack-ranged-done
5 attack-done (for shadow wood)

1: walk-done:
   walk-done
   cancel-done
2: walk-attack-done:
   walk-attack-done
   walk-cancel-done
   cancel-attack-done
   cancel-cancel-done
3: walk-ranged-done:
   walk-ranged-done
   walk-cancel-done
   cancel-ranged-done
   cancel-cancel-done
4: walk-attack-ranged-done:
   walk-attack-ranged-done
   walk-attack-cancel-done
   walk-cancel-ranged-done
   walk-cancel-cancel-done
   cancel-attack-ranged-done
   cancel-attack-cancel-done
   cancel-cancel-cancel-done
5  attack-done:
   attack-done
   cancel-done

for each test, redo with fly instead
also, for each test do 2 squares walk variation:
always walk the first square, the walk or cancel as with above
how to handle shadow wood next to no enemy?

fly only variations?:

multiple square walk variations

what about engaged ish?

seems like loads and loads of tests, can this list be shortened and
still be comprehensive?

walking
flying
walking when engaged
flying when engaged
trying to walk/fly to occupied squares
flying attack
fly then walk attack
walk attack
non undead/undead, magic weapons
shadow form attack
cobra attack dragon
fire ranged weapons


> testMoveSubphases1 conn = TestLabel "testMoveSubphases1" $ TestCase $ do
>     --test 1: move -> attack -> ranged -> unselected
>     startNewGame conn
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     --select dragon
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
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
>                              \  allegiance='Buddha'"
>     assertBool "piece not in ptm" (null v)

> testMoveSubphases2 conn = TestLabel "testMoveSubphases2" $ TestCase $ do
>     --test 2: move -> ranged -> unselected>
>     startNewGame conn
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     --select dragon
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
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
>                              \  allegiance='Buddha'"
>     assertBool "piece not in ptm" (null v)
> testMoveSubphases3 conn = TestLabel "testMoveSubphases3" $ TestCase $ do
>     --test 3: move -> unselected
>     startNewGame conn
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     --select dragon
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
>     checkSelectedPiece conn "Buddha" "giant_rat"
>     checkMoveSubphase conn "motion"
>     sendKeyPress conn "End"
>     --check nothing selected and dragon no longer in ptm table
>     checkNoSelectedPiece conn
>     v <- selectRelationValues conn
>                              "select * from pieces_to_move\n\
>                              \where ptype='giant_rat' and\n\
>                              \  allegiance='Buddha'"
>     assertBool "piece not in ptm" (null v)
> testMoveSubphases4 conn = TestLabel "testMoveSubphases4" $ TestCase $ do
>     --test 4: attack -> unselected
>     startNewGame conn
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     --select dragon
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
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
>                              \  allegiance='Buddha'"
>     assertBool "piece not in ptm" (null v)

> testWalkOneSquare conn = TestLabel "testWalkOneSquare" $ TestCase $ do
>     startNewGame conn
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     sendKeyPress conn "Return"
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
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

> testWalkTwoSquares conn = TestLabel "testWalkTwoSquares" $ TestCase $ do
>     startNewGame conn
>     let pk = (wizardPiecesList ++
>                   [('B',[PieceDescription "bear" "Buddha" []])])
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     --move cursor over bear
>     sendKeyPress conn "Right"
>     --select bear
>     sendKeyPress conn "Return"
>     --move down
>     sendKeyPress conn "Down"
>     sendKeyPress conn "Return"
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
>     sendKeyPress conn "KP_Page_Down"
>     sendKeyPress conn "Return"
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

> testFlyOverPieces conn = TestLabel "testFlyOverPieces" $ TestCase $ do
>     startNewGame conn
>     let pk = (wizardPiecesList ++
>                   [('E', [PieceDescription "eagle" "Buddha" []]),
>                    ('G', [PieceDescription "goblin" "Buddha" []])])
>     setupBoard conn ("\n\
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
>     skipToPhase conn "move"
>     moveCursorTo conn 0 0
>     sendKeyPress conn "Right"
>     sendKeyPress conn "Return"
>     moveCursorTo conn 3 4
>     sendKeyPress conn "Return"
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


> testAttackMonster conn = TestLabel "testAttackMonster" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 0 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "attack" True
>   sendKeyPress conn "Return"
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

> testAttackMonsterResisted conn =
>     TestLabel "testAttackMonsterResisted" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 0 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 0
>   rigActionSuccess conn "attack" False
>   sendKeyPress conn "Return"
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


> testAttackWizard conn = TestLabel "testAttackWizard" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   sendKeyPress conn "space"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 0 0
>   rigActionSuccess conn "attack" True
>   sendKeyPress conn "Return"
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

> testFlyAttack conn = TestLabel "testFlyAttack" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 2 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 3 3
>   rigActionSuccess conn "attack" True
>   sendKeyPress conn "Return"
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

> testFlyThenAttack conn = TestLabel "testFlyThenAttack" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 2 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 2 3
>   sendKeyPress conn "Return"
>   moveCursorTo conn 3 3
>   rigActionSuccess conn "attack" True
>   sendKeyPress conn "Return"
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

> testRangedAttack conn = TestLabel "testRangedAttack" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 2 0
>   sendKeyPress conn "Return"
>   sendKeyPress conn "End"
>   moveCursorTo conn 3 3
>   rigActionSuccess conn "ranged_attack" True
>   sendKeyPress conn "Return"
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

> testRangedAttackResisted conn =
>     TestLabel "testRangedAttackResisted" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 2 0
>   sendKeyPress conn "Return"
>   sendKeyPress conn "End"
>   moveCursorTo conn 3 3
>   rigActionSuccess conn "ranged_attack" False
>   sendKeyPress conn "Return"
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

> testShadowWoodAttack conn = TestLabel "testShadowWoodAttack" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 1
>   rigActionSuccess conn "attack" True
>   sendKeyPress conn "Return"
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

> testDismount conn = TestLabel "testDismount" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 1
>   sendKeyPress conn "Return"
>   checkBoard conn ("\n\
>                   \ P     2      3\n\
>                   \  1            \n\
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

> testSelectWizardUnderneath conn =
>     TestLabel "testSelectWizardUnderneath" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
>                   \P      C      3\n\
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
>                   [('P', [PieceDescription "wizard" "Buddha" [],
>                           PieceDescription "pegasus" "Buddha" []]),
>                    ('C', [PieceDescription "wizard" "Kong Fuzi" [],
>                           PieceDescription "dark_citadel" "Kong Fuzi" []])]))
>   skipToPhase conn "move"
>   moveCursorTo conn 0 0
>   sendKeyPress conn "Return"
>   -- check wizard selected
>   --next turn
>   moveCursorTo conn 7 0
>   sendKeyPress conn "Return"
>   --check wizard selected


-- > testDismount conn = TestLabel "testDismount" $ TestCase $ do
-- >   startNewGame conn
-- >   setupBoard conn ("\n\
-- >                   \ P     2      3\n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \4             5\n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \6      7      8",
-- >                    wizardPiecesList ++
-- >                   [('P', [PieceDescription "wizard" "Buddha" [],
-- >                           PieceDescription "pegasus" "Buddha" []])])
-- >   skipToPhase conn "move"
-- >   moveCursorTo conn 1 0
-- >   sendKeyPress conn "Return"
-- >   moveCursorTo conn 1 1
-- >   sendKeyPress conn "Return"
-- >   checkBoard conn ("\n\
-- >                   \ P     2      3\n\
-- >                   \  1            \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \4             5\n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \               \n\
-- >                   \6      7      8",
-- >                    wizardPiecesList ++
-- >                   [('P', [PieceDescription "pegasus" "Buddha" []])])

> testMount conn = TestLabel "testMount" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>                   [('P', [PieceDescription "pegasus" "Buddha" []])]))
>   skipToPhase conn "move"
>   moveCursorTo conn 0 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
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
>                           PieceDescription "pegasus" "Buddha" []])])

> testMoveWhenMounted conn = TestLabel "testMoveWhenMounted" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
>   sendKeyPress conn "End"
>   sendKeyPress conn "Return"
>   moveCursorTo conn 2 2
>   sendKeyPress conn "Return"
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

> testExit conn = TestLabel "testExit" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 1
>   sendKeyPress conn "Return"
>   checkBoard conn ("\n\
>                   \ P     2      3\n\
>                   \  1            \n\
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

> testEnter conn = TestLabel "testEnter" $ TestCase $ do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>   skipToPhase conn "move"
>   moveCursorTo conn 0 0
>   sendKeyPress conn "Return"
>   moveCursorTo conn 1 0
>   sendKeyPress conn "Return"
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

attack undead - able, able no corpse


attack undead - unable
attack undead - able, no corpse

test monsters in blob cannot be selected, but can be
moved if free'd that turn
 - so check if in selected list, try to select, check failure, then
  kill blob and select and move

shadow form attack and moving, shadow form and magic wings at same time

engaged stuff

wizard upgrades

dead wizard tests


================================================================================

= autonomous action tests

fire, blob spreading
castles disappearing
wizards getting new spell from magic tree


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
>   v <- selectRelation conn "select x,y from current_wizard_spell_squares"
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
>                    let ps = fromJust $ lookup k key
>                    in map (\p -> (p,x,y)) ps)
>

now the code to read the board from the database and get it in the same format:

> readBoard :: Connection -> IO BoardDescription
> readBoard conn =
>   selectRelation conn "select ptype,allegiance,x,y,undead\n\
>                       \from piece_details" >>=
>   convertBoardRel conn

The variant for only the topmost pieces:

> readPiecesOnTop :: Connection -> IO BoardDescription
> readPiecesOnTop conn =
>   selectRelation conn "select ptype,allegiance,x,y,undead\n\
>                           \from pieces_on_top_view" >>=
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
>   v <- selectRelation conn "select wizard_name, magic_armour from wizards"
>   let checkTag r t t' = if r t == "True"
>                           then Just t'
>                           else Nothing
>   return $ for v (\vs -> (vs "wizard_name",
>                                catMaybes [checkTag vs "magic_armour" PArmour]))


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
>   selectRelationValues conn ("select * from " ++ relvarName) >>=
>   assertEqual ("relvar " ++ relvarName) value

shorthands to check data in the database

> checkSelectedPiece conn allegiance ptype = do
>   v <- selectRelation conn "select * from selected_piece"
>   assertEqual "selected piece" (allegiance,ptype)
>               (head v "allegiance", head v "ptype")

> checkMoveSubphase conn sp = do
>   ms <- selectRelation conn "select move_phase from selected_piece"
>   assertEqual "move subphase" sp (head ms "move_phase")

> checkNoSelectedPiece conn = do
>   v <- selectRelation conn "select * from selected_piece"
>   assertBool "no selected piece" (null v)

================================================================================

= setup game functions

== setup board

this takes a board description and sets the board to match it used to
setup a particular game before running some tests

> setupBoard :: Connection -> BoardDiagram -> IO ()
> setupBoard conn bd = do
>   let targetBoard = parseBoardDiagram bd
>   --just assume all the wizards are in the usual starting positions
>   -- and that we just have to add objects
>   --fix this when needed
>   let wizards = [(PieceDescription "wizard" "Buddha" [], 0, 0),
>                  (PieceDescription "wizard" "Kong Fuzi" [] ,7 ,0),
>                  (PieceDescription "wizard" "Laozi" [], 14, 0),
>                  (PieceDescription "wizard" "Moshe" [], 0, 4),
>                  (PieceDescription "wizard" "Muhammad" [], 14,4),
>                  (PieceDescription "wizard" "Shiva" [], 0, 9),
>                  (PieceDescription "wizard" "Yeshua" [], 7, 9),
>                  (PieceDescription "wizard" "Zarathushthra" [], 14,9)]
>       nonWizardItems = flip filter targetBoard (\x -> not (x `elem` wizards))
>   let addObject (PieceDescription object allegiance tags, x, y) =
>         if allegiance == "dead"
>           then callSp conn "create_corpse"
>                       [object,
>                        (show x),
>                        (show y),
>                        show (PImaginary `elem` tags)]
>           else callSp conn "create_piece_internal"
>                       [object,
>                        allegiance,
>                        (show x),
>                        (show y),
>                        show (PImaginary `elem` tags)]
>   mapM_ addObject nonWizardItems
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
>   r <- selectRelation conn "select turn_phase from turn_phase_table"
>   return $ head r "turn_phase"

> readCurrentWizard :: Connection -> IO String
> readCurrentWizard conn = do
>   r <- selectRelation conn "select current_wizard from current_wizard_table"
>   return $ head r "current_wizard"
