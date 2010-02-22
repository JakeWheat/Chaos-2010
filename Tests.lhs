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
piece starting ticks
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

> {-# LANGUAGE ScopedTypeVariables #-}
> import Data.List
> import qualified Data.Map as M
> --import qualified Data.Char as DC
> import Control.Monad
> import Data.Maybe
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Exception

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HaskellDB
> import Database.HDBC.PostgreSQL
> import Database.HDBC

> import Games.Chaos2010.Tests.BoardUtils
> --import ChaosDB
> --import Games.Chaos2010.Conf
> --import Games.Chaos2010.Utils
> --import qualified Games.Chaos2010.Misc.Logging as Logging
> --import Games.Chaos2010.Database
> --import Games.Chaos2010.Database.Pieces_mr

================================================================================

= Main

Run all the tests.

> main :: IO ()
> main = do -- time $ do
>   -- Logging.setupLogging
>   -- conf <- getConfig
>   postgresqlConnect [("dbname", "chaos")] $ \db -> do
>   withConn ("dbname=chaos") $ \conn -> do
>   resetNewGameWidgetState conn
>   setAllHuman conn
>   defaultMain [
>         testGroup "basics" [
>                        --testDatabaseStuff conn
>                        testCursorMovement db conn
>                       ,testPiecesOnTop db conn
>                       ]
>        ,testGroup "phases" [
>                        testNextPhase db conn
>                       ,testNextPhaseWizardDead db conn
>                       ,testNextPhaseTwoWizardsDead db conn
>                       --,testNextPhaseAI db conn
>                       ]

>        ,testGroup "casting" [
>                        testCastGoblin db conn
>                       ,testFailCastGoblin db conn
>                       ,testCastMagicWood db conn
>                       ,testCastShadowWood db conn
>                       ,testCastMagicBolt db conn
>                       ,testCastMagicBoltResisted db conn
>                       ,testCastVengeanceWizard db conn
>                       ,testCastVengeanceMonster db conn
>                       ,testCastVengeanceMonsterResisted db conn
>                       ,testCastSubversion db conn
>                       ,testCastSubversionResisted db conn
>                       ,testCastDisbelieveReal db conn
>                       ,testCastDisbelieveImaginary db conn
>                       ,testCastRaiseDead db conn
>                       ,testCastLaw db conn
>                       ,testImaginary db conn
>                       ,testGroup "upgrades" [
>                                       testCastShield db conn
>                                      ,testCastArmour db conn
>                                      ,testCastKnife db conn
>                                      ,testCastSword db conn
>                                      ,testCastBow db conn
>                                      ,testCastWings db conn
>                                      ,testCastShadowForm db conn
>                                      ]
>                       ]
>        ,testGroup "autonomous" [
>                        testCastleDisappear db conn
>                       ,testCastleStay db conn
>                       ,testGetSpell db conn
>                       ,testNoGetSpell db conn
>                       ]
>        ,testGroup "move phase stuff" [
>           testGroup "subphases" [
>                          testWalkCancelAttackDone db conn
>                         ,testCancelMotionDone db conn
>                         ,testWalkNoAvailAttackDone db conn
>                         ,testWAttackDone db conn
>                         ,testWalk2Done db conn
>                         ,testWalk1CancelDone db conn
>                         ,testFlyAttackDone db conn
>                         ,testFAttackDone db conn
>                         ,testCancelFlyDone db conn
>                         ,testFlyNoAvailAttackDone db conn
>                         ,testWalkAttackRangedDone db conn
>                         ,testWalkAttackCancelDone db conn
>                         ,testWalkCancelRangedDone db conn
>                         ,testCancelRangedDone db conn
>                         ,testWalkNoAvailAttackRangedDone db conn
>                         ,testWalkStraightRangedDone db conn
>                         ,testStraightRangedDone db conn
>                         ,testAttackDone db conn
>                         ,testNoAvailAttackDone db conn
>                         ,testAttackNonUndeadOnUndead db conn
>                         ,testMagicWeaponOnUndead db conn
>                         ,testAttackUndeadOnUndead db conn
>                         ,testNoMoveEngaged db conn
>                         ,testBreakEngaged db conn
>                         ]
>           ,testGroup "moveMisc" [
>                           testAttackWizard db conn
>                          ,testFlyAttack db conn
>                          ,testMountThenMoveMount db conn
>                          ,testMoveWhenMounted db conn
>                          ,testDismount db conn
>                          ,testMoveWhenAlreadyMounted db conn
>                          ,testEnter db conn
>                          ,testExit db conn
>                          ,testAttackShadowForm db conn
>                          ,testBlobSelection db conn
>                        ]
>           ]
>        ,testGroup "game complete" [
>                        testWizardWin db conn
>                       ,testGameDraw db conn
>                       ]
>                 ]

>   return ()

> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (connectPostgreSQL cs)
>                         disconnect
>                         f


> tctor :: String
>       -> (Connection -> IO ())
>       -> Connection
>       -> Test.Framework.Test
> tctor l f conn = testCase l $ rollbackOnError conn $ f conn

postgresql.conf  #track_functions = none # none, pl, all

 select * from pg_stat_user_functions ;
http://www.depesz.com/index.php/2008/05/15/waiting-for-84-function-stats/

> {-testHaskellDB :: Database -> IO ()
> testHaskellDB db = do
>   putStrLn "here"
>   {-let q = do u <- table pieces
>           restrict ((u!name .==. user) .&&.(u!password .==. passwd))
>           project (rights << u!rights) -}
>   res <- query db $ table pieces_mr --q
>   mapM_ putStrLn $ map convRow res
>   return ()
>   where
>     --convRow :: Record a -> String
>     convRow r = show $ r # ptype-}

undefined


================================================================================

= Tests

== tests in sql

First run the tests from the database, these are all the database
functions whose name starts with 'check_code_'

> {-testDatabaseStuff :: Connection -> Test.Framework.Test
> testDatabaseStuff db = tctor "testDatabaseStuff" $ \conn -> do
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

>   return ()-}


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

> testPiecesOnTop :: Database -> Connection -> Test.Framework.Test
> testPiecesOnTop db = tctor "testPiecesOnTop" $ \conn -> do
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
>                   (wizardPiecesList ++ liftPl
>                    [('a', [("goblin", "Kong Fuzi")
>                           ,("elf", "dead")])
>                    ,('b', [("goblin", "dead")
>                           ,("wizard", "Buddha")])
>                    ,('c', [("pegasus", "Kong Fuzi")
>                           ,("giant", "dead")
>                           ,("wizard", "Kong Fuzi")])
>                    ,('d', [("gryphon", "Laozi")
>                           ,("wizard", "Laozi")])
>                    ,('e', [("magic_castle", "Moshe")
>                           ,("wizard", "Moshe")])
>                    ,('f', [("magic_tree", "Muhammad")
>                           ,("wizard", "Muhammad")])
>                    ,('g', [("goblin", "dead")
>                           ,("gooey_blob", "Buddha")])
>                    ,('h', [("goblin", "Kong Fuzi")
>                           ,("gooey_blob", "Buddha")])
>                    ,('i', [("goblin", "Kong Fuzi")
>                           ,("gooey_blob", "Buddha")
>                           ,("elf", "dead")])]))
>   assertTopPiecesEquals db ("\n\
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
>                   (wizardPiecesList ++ liftPl
>                    [('a', [("goblin","Kong Fuzi")]),
>                     ('b', [("wizard","Buddha")]),
>                     ('c', [("pegasus","Kong Fuzi")]),
>                     ('d', [("gryphon","Laozi")]),
>                     ('e', [("magic_castle","Moshe")]),
>                     ('f', [("magic_tree","Muhammad")]),
>                     ('g', [("gooey_blob","Buddha")]),
>                     ('h', [("gooey_blob","Buddha")]),
>                     ('i', [("gooey_blob","Buddha")])]))


== cursor movement

Check the cursor movement and also the shortcut for the tests to move
the cursor to a given position, also check the moveto code

> testCursorMovement :: Database -> Connection -> Test.Framework.Test
> testCursorMovement db = tctor "testCursorMovement" $ \conn -> do
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
> cursorShorthand m = undefined {-safeLookup "cursor shorthand" m
>                      [("d", "Down"),
>                       ("l", "Left"),
>                       ("u", "Up"),
>                       ("r", "Right"),
>                       ("dl", "KP_End"),
>                       ("dr", "KP_Page_Down"),
>                       ("ul", "KP_Home"),
>                       ("ur", "KP_Page_Up")]-}

get the current cursor position from the database

> readCursorPosition :: Connection -> IO (Int,Int)
> readCursorPosition conn = do
>   undefined
>   --r <- selectTuple conn "select x,y from cursor_position" []
>   --return (read $ lk "x" r, read $ lk "y" r)

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

> testNextPhase :: Database -> Connection -> Test.Framework.Test
> testNextPhase db = tctor "testNextPhase" $ \conn -> do
>   startNewGame conn
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                checkCurrentWizardPhase conn (wizardNames !! i) phase
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                undefined
>                {-whenA1 (readTurnPhase conn)
>                       (=="choose")
>                       (sendKeyPress conn "Q")-}
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead :: Database -> Connection -> Test.Framework.Test
> testNextPhaseWizardDead db = tctor "testNextPhaseWizardDead" $ \conn ->
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizardNames !! j]
>     let theseWizards = undefined -- dropItemN wizardNames j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>         checkCurrentWizardPhase conn (theseWizards !! i) phase
>         undefined {-whenA1 (readTurnPhase conn)
>                (=="choose")
>                (sendKeyPress conn "Q")-}
>         sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead :: Database -> Connection -> Test.Framework.Test
> testNextPhaseTwoWizardsDead db = tctor "testNextPhaseTwoWizardsDead" $ \conn ->
>   forM_ [0..7] (\j ->
>     forM_ [(j + 1)..7] (\k -> do
>       startNewGame conn
>       --kill wizards
>       callSp conn "kill_wizard" [wizardNames !! j]
>       callSp conn "kill_wizard" [wizardNames !! k]
>       let theseWizards = undefined {-dropItemN (dropItemN wizardNames k) j-}
>       forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>         forM_ [0..5] (\i -> do
>           checkCurrentWizardPhase conn (theseWizards !! i) phase
>           --so we don't skip the cast phase, make sure
>           -- each wizard has a spell chosen, use disbelieve
>           --cos wizards always have this spell available
>           undefined
>           {-whenA1 (readTurnPhase conn)
>                  (=="choose")
>                  (sendKeyPress conn "Q")-}
>           sendKeyPress conn "space"))))


check wizards dying during move when it is their turn - this can
happen if you shoot your own wizard with a ranged weapon from a
monster, the game should cope with it - I think this is tested in the
game drawn test

automatic next phase tests:
casting the last part of a spell moves to the next player automatically
moving the last creature moves to the next player automatically
these are tested in the spell cast and move sections respectively

> {-testNextPhaseAI :: Database -> Connection -> Test.Framework.Test
> testNextPhaseAI db = tctor "testNextPhaseAI" $ \conn -> do
>   startNewGameAI conn
>   forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>     forM_ [0..7] (\i -> do
>        checkCurrentWizardPhase conn (wizardNames !! i) phase
>        sendKeyPress conn "space"))-}


================================================================================

= spell cast tests

> testCastGoblin :: Database -> Connection -> Test.Framework.Test
> testCastGoblin db = tctor "testCastGoblin" $ \conn -> do

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

>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Buddha"])]))
>   --check we are on the next wizard's phase
>   checkCurrentWizardPhase conn "Kong Fuzi" "cast"

> testFailCastGoblin :: Database -> Connection -> Test.Framework.Test
> testFailCastGoblin db = tctor "testFailCastGoblin" $ \conn -> do
>   newGameReadyToCast conn "goblin"
>   rigActionSuccess conn "cast" False
>   goSquare conn 1 0
>   assertBoardEquals db ("\n\
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

> testCastMagicWood :: Database -> Connection -> Test.Framework.Test
> testCastMagicWood db = tctor "testCastMagicWood" $ \conn -> do
>   newGameReadyToCast conn "magic_wood"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   assertBoardEquals db ("\n\
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
>                   [('W', [makePD "magic_tree" "Buddha"])]))

> testCastShadowWood :: Database -> Connection -> Test.Framework.Test
> testCastShadowWood db = tctor "testCastShadowWood" $ \conn -> do
>   newGameReadyToCast conn "shadow_wood"
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('W', [makePD "shadow_tree" "Buddha"])]))

> testCastMagicBolt :: Database -> Connection -> Test.Framework.Test
> testCastMagicBolt db = tctor "testCastMagicBolt" $ \conn -> do
>   newGameReadyToCast conn "magic_bolt"
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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

> testCastMagicBoltResisted :: Database -> Connection -> Test.Framework.Test
> testCastMagicBoltResisted db = tctor "testCastMagicBoltResisted" $ \conn -> do
>   newGameReadyToCast conn "magic_bolt"
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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

> testCastVengeanceWizard :: Database -> Connection -> Test.Framework.Test
> testCastVengeanceWizard db = tctor "testCastVengeanceWizard" $ \conn -> do
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
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" Real Alive])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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

> testCastVengeanceMonster :: Database -> Connection -> Test.Framework.Test
> testCastVengeanceMonster db = tctor "testCastVengeanceMonster" $ \conn -> do
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
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" Real Alive])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))

> testCastVengeanceMonsterResisted :: Database -> Connection -> Test.Framework.Test
> testCastVengeanceMonsterResisted db =
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
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" Real Alive])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))

> testCastSubversion :: Database -> Connection -> Test.Framework.Test
> testCastSubversion db = tctor "testCastSubversion" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Buddha"])]))

> testCastSubversionResisted :: Database -> Connection -> Test.Framework.Test
> testCastSubversionResisted db = tctor "testCastSubversionResisted" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))

> testCastDisbelieveReal :: Database -> Connection -> Test.Framework.Test
> testCastDisbelieveReal db = tctor "testCastDisbelieveReal" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))

> testCastDisbelieveImaginary :: Database -> Connection -> Test.Framework.Test
> testCastDisbelieveImaginary db = tctor "testCastDisbelieveImaginary" $ \conn -> do
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
>                   [('G', [PieceDescription "goblin" "Kong Fuzi" Imaginary Alive])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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

> testCastRaiseDead :: Database -> Connection -> Test.Framework.Test
> testCastRaiseDead db = tctor "testCastRaiseDead" $ \conn -> do
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
>                   [('G', [makePD "goblin" "dead"])]))
>   assertValidSquaresEquals db "\n\
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
>   assertBoardEquals db ("\n\
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
>                   [('G', [PieceDescription "goblin" "Buddha" Real Undead])]))

> testCastShield :: Database -> Connection -> Test.Framework.Test
> testCastShield db = tctor "testCastShield" $ \conn ->
>   doUpgradeTest conn db "magic_shield" $
>                 addStat "physical_defense" 2

> testCastArmour :: Database -> Connection -> Test.Framework.Test
> testCastArmour db = tctor "testCastArmour" $ \conn ->
>   doUpgradeTest conn db "magic_armour" $
>                 addStat "physical_defense" 4

> testCastKnife :: Database -> Connection -> Test.Framework.Test
> testCastKnife db = tctor "testCastKnife" $ \conn ->
>   doUpgradeTest conn db "magic_knife" $
>                 addStat "attack_strength" 2

> testCastSword :: Database -> Connection -> Test.Framework.Test
> testCastSword db = tctor "testCastSword" $ \conn ->
>   doUpgradeTest conn db "magic_sword" $
>                 addStat "attack_strength" 4

> testCastBow :: Database -> Connection -> Test.Framework.Test
> testCastBow db = tctor "testCastBow" $ \conn ->
>   doUpgradeTest conn db "magic_bow" $
>                 setStat "ranged_weapon_type" "projectile" .
>                 setStat "range" "6" .
>                 setStat "ranged_attack_strength" "6"

> testCastWings :: Database -> Connection -> Test.Framework.Test
> testCastWings db = tctor "testCastWings" $ \conn ->
>   doUpgradeTest conn db "magic_wings" $
>                 setStat "speed" "6" .
>                 setStat "flying" "True"

> testCastShadowForm :: Database -> Connection -> Test.Framework.Test
> testCastShadowForm db = tctor "testCastShadowForm" $ \conn ->
>   doUpgradeTest conn db "shadow_form" $
>                 addStat "physical_defense" 2 .
>                 addStat "agility" 2 .
>                 setStat "speed" "3"

> addStat :: String -> Int -> SqlRow -> SqlRow
> addStat att n tup = M.insert att (adds $ fromJust $ M.lookup att tup) tup
>     where
>       adds s = show $ (read s ::Int) + n

> setStat :: String -> String -> SqlRow -> SqlRow
> setStat = M.insert

> doUpgradeTest :: Connection -> Database -> String -> (SqlRow -> SqlRow)
>               -> IO ()
> doUpgradeTest conn db spell attrChange = do
>   newGameReadyToCast conn spell
>   undefined {-
>   oldStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []-}
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   undefined
>   {-newStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'" []-}
>   undefined --assertEqual "upgraded stats" (attrChange oldStats) undefined newStats
>   assertBoardEquals db ("\n\
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

> type SqlRow = M.Map String String

todo:
stack upgrades - different spells, same spells e.g. magic knife
twice, magic knife then magic sword

if there is a way that a wizard can lose an upgrade that needs to be
tested too


> testCastLaw :: Database -> Connection -> Test.Framework.Test
> testCastLaw db = tctor "testCastLaw" $ \conn -> do
>   newGameReadyToCast conn "law"
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   undefined {-
>   align <- selectValue conn "select world_alignment\n\
>                               \from world_alignment_table" []
>   assertEqual "world alignment not law after casting law" "1" align-}

> testImaginary :: Database -> Connection -> Test.Framework.Test
> testImaginary db = tctor "testImaginary" $ \conn -> do

>   let setStuffUp1 = do
>                     startNewGame conn
>                     addSpell conn "goblin"
>                     sendKeyPress conn $ keyChooseSpell "goblin"
>   let setStuffUp2 = do
>                     skipToPhase conn "cast"
>                     rigActionSuccess conn "cast" True
>                     goSquare conn 1 0
>   let sv c = undefined {-do
>              v <- selectValue conn "select imaginary from monster_pieces\n\
>                                    \natural inner join pieces\n\
>                                    \where x= 1 and y = 0" []
>              c ((read v) :: Bool)-}
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

> testWalkCancelAttackDone :: Database -> Connection -> Test.Framework.Test
> testWalkCancelAttackDone db = tctor "testWalkCancelAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testCancelMotionDone :: Database -> Connection -> Test.Framework.Test
> testCancelMotionDone db = tctor "testCancelMotionDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testWalkNoAvailAttackDone :: Database -> Connection -> Test.Framework.Test
> testWalkNoAvailAttackDone db = tctor "testWalkNoAvailAttackDone" $
>                                    \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testWAttackDone :: Database -> Connection -> Test.Framework.Test
> testWAttackDone db = tctor "testWAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('E', [makePD "goblin" "Buddha",
>                       makePD "giant_rat" "dead"])])
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
>     assertBoardEquals db ("\n\
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

> testWalk2Done :: Database -> Connection -> Test.Framework.Test
> testWalk2Done db = tctor "testWalk2Done" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [makePD "bear" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testWalk1CancelDone :: Database -> Connection -> Test.Framework.Test
> testWalk1CancelDone db = tctor "testWalk1CancelDone" $
>                              \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [makePD "bear" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testFlyAttackDone :: Database -> Connection -> Test.Framework.Test
> testFlyAttackDone db = tctor "testFlyAttackDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "eagle" "Buddha",
>                       makePD "giant_rat" "dead"])])
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
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testFAttackDone :: Database -> Connection -> Test.Framework.Test
> testFAttackDone db = tctor "testFAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testCancelFlyDone :: Database -> Connection -> Test.Framework.Test
> testCancelFlyDone db = tctor "testCancelFlyDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testFlyNoAvailAttackDone :: Database -> Connection -> Test.Framework.Test
> testFlyNoAvailAttackDone db = tctor "testFlyNoAvailAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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

> testWalkAttackRangedDone :: Database -> Connection -> Test.Framework.Test
> testWalkAttackRangedDone db = tctor "testWalkAttackRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "elf" "Buddha",
>                       makePD "giant_rat" "dead"]),
>                ('r', [makePD "giant_rat" "dead"])])
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
>     assertBoardEquals db ("\n\
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
>     checkMoveSubphase conn "ranged_attack"
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testWalkAttackCancelDone :: Database -> Connection -> Test.Framework.Test
> testWalkAttackCancelDone db = tctor "testWalkAttackCancelDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "elf" "Buddha",
>                       makePD "giant_rat" "dead"])])
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
>     assertBoardEquals db ("\n\
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
>     checkMoveSubphase conn "ranged_attack"
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testWalkCancelRangedDone :: Database -> Connection -> Test.Framework.Test
> testWalkCancelRangedDone db = tctor "testWalkCancelRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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
>     checkMoveSubphase conn "ranged_attack"
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testCancelRangedDone :: Database -> Connection -> Test.Framework.Test
> testCancelRangedDone db = tctor "testCancelRangedDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('r', [makePD "giant_rat" "dead"])])
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
>     checkMoveSubphase conn "ranged_attack"
>     assertBoardEquals db ("\n\
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
>     assertBoardEquals db ("\n\
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

> testWalkNoAvailAttackRangedDone :: Database -> Connection -> Test.Framework.Test
> testWalkNoAvailAttackRangedDone db =
>   tctor "testWalkNoAvailAttackRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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
>     assertBoardEquals db ("\n\
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
>     checkMoveSubphase conn "ranged_attack"
>     assertBoardEquals db ("\n\
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
>     goSquare conn 4 1
>     checkPieceDoneSelection conn "elf" "Buddha"
>     assertBoardEquals db ("\n\
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

> testWalkStraightRangedDone :: Database -> Connection -> Test.Framework.Test
> testWalkStraightRangedDone db =
>   tctor "testWalkStraightRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \   RR          \n\
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
>     assertBoardEquals db ("\n\
>                   \1 G R  2      3\n\
>                   \   RR          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkMoveSubphase conn "attack"
>     assertBoardEquals db ("\n\
>                   \1 G R  2      3\n\
>                   \   RR          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     rigActionSuccess conn "ranged_attack" False
>     goSquare conn 4 1
>     checkPieceDoneSelection conn "elf" "Buddha"
>     assertBoardEquals db ("\n\
>                   \1 G R  2      3\n\
>                   \   RR          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testStraightRangedDone :: Database -> Connection -> Test.Framework.Test
> testStraightRangedDone db =
>   tctor "testStraightRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     startNewGameReadyToMove conn ("\n\
>                   \1G  R  2      3\n\
>                   \  R R          \n\
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
>     rigActionSuccess conn "ranged_attack" False
>     goSquare conn 4 0
>     assertBoardEquals db ("\n\
>                   \1G  R  2      3\n\
>                   \  R R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>     checkPieceDoneSelection conn "elf" "Buddha"
>     assertBoardEquals db ("\n\
>                   \1G  R  2      3\n\
>                   \  R R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)



* piece has attack only

> testAttackDone :: Database -> Connection -> Test.Framework.Test
> testAttackDone db = tctor "testAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [makePD "shadow_tree" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('r', [makePD "giant_rat" "dead"])])
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
>     assertBoardEquals db ("\n\
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

> testNoAvailAttackDone :: Database -> Connection -> Test.Framework.Test
> testNoAvailAttackDone db = tctor "testNoAvailAttackDone" $
>                                \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [makePD "shadow_tree" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
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

> testAttackWizard :: Database -> Connection -> Test.Framework.Test
> testAttackWizard db = tctor "testAttackWizard" $
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
>                   [('G', [makePD "goblin" "Kong Fuzi"]),
>                    ('H', [makePD "goblin" "Buddha"])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   assertBoardEquals db ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))

> testFlyAttack :: Database -> Connection -> Test.Framework.Test
> testFlyAttack db = tctor "testFlyAttack" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"]),
>                    ('E', [makePD "eagle" "Buddha"])]))
>   goSquare conn 2 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 3 3
>   assertBoardEquals db ("\n\
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
>                   [('E', [makePD "eagle" "Buddha",
>                           makePD "goblin" "dead"])]))

> testMountThenMoveMount :: Database -> Connection -> Test.Framework.Test
> testMountThenMoveMount db = tctor "testMountThenMoveMount" $
>                              \conn -> do
>   let pl = wizardPiecesList ++ [('P', [makePD "pegasus" "Buddha"]),
>             ('M', [makePD "wizard" "Buddha",
>                    makePD "pegasus" "Buddha"])]
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
>   assertBoardEquals db ("\n\
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

> testMoveWhenMounted :: Database -> Connection -> Test.Framework.Test
> testMoveWhenMounted db = tctor "testMoveWhenMounted" $
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "pegasus" "Buddha"])])
>   --select then cancel the wizard
>   goSquare conn 1 0
>   assertSelectedPiece conn "wizard" "Buddha"
>   sendKeyPress conn "End"
>   --now we can select the monster
>   sendKeyPress conn "Return"
>   assertSelectedPiece conn "pegasus" "Buddha"
>   goSquare conn 2 2
>   assertBoardEquals db ("\n\
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "pegasus" "Buddha"])])

dismount then move

> testDismount :: Database -> Connection -> Test.Framework.Test
> testDismount db = tctor "testDismount" $ \conn -> do
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "pegasus" "Buddha"])])
>   goSquare conn 1 0
>   goSquare conn 1 1
>   goSquare conn 1 0
>   goSquare conn 3 0
>   assertBoardEquals db ("\n\
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
>                   [('P', [makePD "pegasus" "Buddha"])])

move when already mounted

> testMoveWhenAlreadyMounted :: Database -> Connection -> Test.Framework.Test
> testMoveWhenAlreadyMounted db = tctor "testMoveWhenAlreadyMounted" $
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "pegasus" "Buddha"])])
>   goSquare conn 1 0
>   sendKeyPress conn "End"
>   sendKeyPress conn "Return"
>   goSquare conn 3 0
>   assertBoardEquals db ("\n\
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "pegasus" "Buddha"])])

todo: attack when dismounting, dismounting when flying

> testExit :: Database -> Connection -> Test.Framework.Test
> testExit db = tctor "testExit" $ \conn -> do
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "dark_citadel" "Buddha"])])
>   goSquare conn 1 0
>   assertSelectedPiece conn "wizard" "Buddha"
>   goSquare conn 1 1
>   assertBoardEquals db ("\n\
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
>                   [('P', [makePD "dark_citadel" "Buddha"])])

> testEnter :: Database -> Connection -> Test.Framework.Test
> testEnter db = tctor "testEnter" $ \conn -> do
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
>                   [('P', [makePD "dark_citadel" "Buddha"])]))
>   goSquare conn 0 0
>   goSquare conn 1 0
>   assertBoardEquals db ("\n\
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
>                   [('P', [makePD "wizard" "Buddha",
>                           makePD "dark_citadel" "Buddha"])])

> testAttackShadowForm :: Database -> Connection -> Test.Framework.Test
> testAttackShadowForm db = tctor "testAttackShadowForm" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   undefined
>   {-oldStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'"-}
>   sendKeyPress conn "Return"
>   skipToPhase conn "move"
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" False
>   goSquare conn 1 0
>   undefined
>   {-newStats <- selectTuple conn "select * from pieces_mr\n\
>                               \  where ptype='wizard'\n\
>                               \    and allegiance='Buddha'"
>   assertEqual "attacking loses shadow form" oldStats newStats-}


> testAttackUndeadOnUndead :: Database -> Connection -> Test.Framework.Test
> testAttackUndeadOnUndead db = tctor "testAttackUndeadOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "goblin" "Buddha" Real Undead])]
>   startNewGameReadyToMove conn ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 1
>   assertBoardEquals db ("\n\
>                   \1      2      3\n\
>                   \ S             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)

> testAttackNonUndeadOnUndead :: Database -> Connection -> Test.Framework.Test
> testAttackNonUndeadOnUndead db = tctor "testAttackNonUndeadOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "goblin" "Buddha" Real Alive])]
>   startNewGameReadyToMove conn ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 1
>   assertBoardEquals db ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)

> testMagicWeaponOnUndead :: Database -> Connection -> Test.Framework.Test
> testMagicWeaponOnUndead db = tctor "testMagicWeaponOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "spectre" "Buddha" Real Undead])]
>   startNewGameReadyToMove conn ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   runSql conn "update wizards set magic_sword = true\n\
>               \where wizard_name='Buddha'" []
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 1
>   assertBoardEquals db ("\n\
>                   \ S     2      3\n\
>                   \ 1             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)


== misc todo

> testNoMoveEngaged :: Database -> Connection -> Test.Framework.Test
> testNoMoveEngaged db = tctor "testNoMoveEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "goblin" "Kong Fuzi" Real Undead])]
>   startNewGameReadyToMove conn ("\n\
>                   \1      2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "break_engaged" False
>   goSquare conn 0 0
>   goSquare conn 0 1
>   assertBoardEquals db ("\n\
>                   \1      2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)

> testBreakEngaged :: Database -> Connection -> Test.Framework.Test
> testBreakEngaged db = tctor "testBreakEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "goblin" "Kong Fuzi" Real Undead])]
>   startNewGameReadyToMove conn ("\n\
>                   \1      2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "break_engaged" True
>   goSquare conn 0 0
>   goSquare conn 0 1
>   assertBoardEquals db ("\n\
>                   \       2      3\n\
>                   \1G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)

engaged stuff: engaged at start of move, and becoming engaged part way
through walk for multiwalkers

second monster dying on a square - the first corpse should disappear permanently

check moving 2 diagonal squares uses up three squares to move

check mount ends motion subphase for wizard

---

check when moving a mounted wizard, that the corpse on that square stays put

trying to walk/fly to occupied squares

cobra attack dragon

test select one creature and move, then select a second creature and move


test monsters in blob cannot be selected, but can be
moved if free'd that turn
 - so check if in selected list, try to select, check failure, then
  kill blob and select and move

> testBlobSelection :: Database -> Connection -> Test.Framework.Test
> testBlobSelection db = tctor "testBlobSelection" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "goblin" "Buddha",
>                    makePD "gooey_blob" "Kong Fuzi"]),
>             ('g', [makePD "goblin" "Buddha"])]
>   startNewGameReadyToMoveNoSpread conn ("\n\
>                   \1O     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   goSquare conn 1 0
>   checkNoSelectedPiece conn
>   goSquare conn 0 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 1 0
>   checkNoSelectedPiece conn
>   assertBoardEquals db ("\n\
>                   \1g     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>   goSquare conn 1 0
>   checkSelectedPiece conn "Buddha" "goblin"


check attack when has mount moves wizard
check attack,nter,mount from mount


> startNewGameReadyToMove :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToMove conn board = do
>   startNewGame conn
>   setupBoard conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase conn "move"

> startNewGameReadyToMoveNoSpread :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToMoveNoSpread conn board = do
>   startNewGame conn
>   runSql conn "update disable_spreading_table\n\
>               \set disable_spreading = true;" []
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

test single square disappear

test spreading onto square with pieces

castles disappearing

> testCastleDisappear :: Database -> Connection -> Test.Framework.Test
> testCastleDisappear db = tctor "testCastleDisappear" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
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
>   assertBoardEquals db ("\n\
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

> testCastleStay :: Database -> Connection -> Test.Framework.Test
> testCastleStay db = tctor "testCastleStay" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
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
>   assertBoardEquals db ("\n\
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
> selectInt conn q a = undefined {-do
>     x <- selectValue conn q a
>     return (read x ::Int)-}

> testGetSpell :: Database -> Connection -> Test.Framework.Test
> testGetSpell db = tctor "testGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
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
>   assertBoardEquals db ("\n\
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

> testNoGetSpell :: Database -> Connection -> Test.Framework.Test
> testNoGetSpell db = tctor "testNoGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
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
>   assertBoardEquals db ("\n\
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

> testWizardWin :: Database -> Connection -> Test.Framework.Test
> testWizardWin db = tctor "testWizardWin" $ \conn -> do
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   sendKeyPress conn "space"
>   assertRelvarValue "game won history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game_won';" []
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
> assertRelvarValue m conn q args val = undefined {-do
>   v <- selectValue conn q args
>   assertEqual m val (read v)-}

Draw works similarly to win, except it is detected the instant there
are no wizards left.

> testGameDraw :: Database -> Connection -> Test.Framework.Test
> testGameDraw db = tctor "testGameDraw" $ \conn -> do
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
>                   [('G', [makePD "green_dragon" "Kong Fuzi"])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   --now magically kill the remaining to force the draw
>   runSql conn "select kill_top_piece_at(?,?);" ["3","0"]
>   assertRelvarValue "game drawn history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game_drawn';" []
>                     (1::Int)
>   assertRelvarValue "now valid target actions"
>                     conn "select count(1)\n\
>                          \from client_valid_target_actions;" []
>                     (0::Int)
>   assertRelvarValue "now valid activate actions"
>                     conn "select count(1)\n\
>                          \from client_valid_activate_actions;" []
>                     (0::Int)

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
> checkRelvar conn relvarName value = undefined
>   {-selectTuplesC conn ("select * from " ++ relvarName) []
>                      (\r -> head $ map snd $ M.toList r) >>=
>     assertEqual ("relvar " ++ relvarName) value-}

shorthands to check data in the database

> checkSelectedPiece :: Connection -> [Char] -> [Char] -> IO ()
> checkSelectedPiece conn allegiance ptype = undefined {-do
>   v <- selectTuple conn "select * from selected_piece"
>   assertEqual "selected piece" (allegiance,ptype)
>               (lk "allegiance" v, lk "ptype" v)-}

> checkMoveSubphase :: Connection -> [Char] -> IO ()
> checkMoveSubphase conn sp = undefined {-do
>   ms <- selectTuple conn "select move_phase from selected_piece"
>   assertEqual "move subphase" sp (lk "move_phase" ms)-}

> checkNoSelectedPiece :: Connection -> IO ()
> checkNoSelectedPiece conn = undefined {-do
>   v <- selectTupleIf conn "select * from selected_piece"
>   assertBool ("there should be no selected piece, got " ++
>               let r =  fromJust v
>               in lk "ptype" r ++ " - " ++ lk "allegiance" r ++
>                  " " ++ lk "move_phase" r)
>              (isNothing v)-}

================================================================================

= setup game functions

== setup board

this takes a board description and sets the board to match it used to
setup a particular game before running some tests

> setupBoard :: Connection -> BoardDiagram -> IO ()
> setupBoard conn bd = undefined {-do
>   let targetBoard = parseBoardDiagram bd
>   --just assume that present wizards are in the usual starting positions
>   --fix this when needed
>   let (wizardItems, nonWizardItems) =
>          partition (\(makePD t _ _, _, _) -> t == "wizard")
>          targetBoard
>       isWizPresent name = any (\(makePD _ n _, _, _) ->
>                                n == name) wizardItems
>   -- remove missing wizards
>   forM_ [0..7] (\i ->
>     unless (isWizPresent $ wizardNames !! i) $
>       callSp conn "kill_wizard" [wizardNames !! i])
>   -- move present wizards
>   forM_ wizardItems (\(makePD _ name _, x, y) -> do
>     t <- selectTuple conn "select x,y from pieces where ptype='wizard'\n\
>                      \and allegiance=?" [name]
>     unless (read (lk "x" t) == x && read (lk "y" t) == y)
>            (runSql conn "update pieces set x = ?, y = ?\n\
>                        \where ptype='wizard' and allegiance=?"
>                    [show x, show y, name]))
>   -- add extra pieces
>   forM_ nonWizardItems $ \(makePD ptype allegiance tags, x, y) -> do
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
>                    show (PImaginary `elem` tags)]
>     when (PUndead `elem` tags) $ do
>       tag <- selectValue conn "select max(tag) from pieces"
>       callSp conn "make_piece_undead" [ptype,allegiance, tag]
>   return () -}

this overrides the next random test by name e.g. so we can test the
board after a spell has succeeded and after it has failed

> rigActionSuccess :: Connection -> String -> Bool -> IO ()
> rigActionSuccess conn override setting = do
>   dbAction conn "rig_action_success" [override, show setting]
>   dbAction conn "reset_current_effects" []

================================================================================

= database update helpers

> {-startNewGame :: Connection -> IO ()
> startNewGame conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='human'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}

> {-startNewGameAI :: Connection -> IO ()
> startNewGameAI conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='computer'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}


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
>   undefined
>   {-whenA1 (readTurnPhase conn)
>          (/= phase) $ do
>          sendKeyPress conn "space"
>          skipToPhase conn phase-}

> sendKeyPress :: Connection -> String -> IO ()
> sendKeyPress conn k = do
>   dbAction conn "key_pressed" [k]
>   dbAction conn "reset_current_effects" []

> rollbackOnError :: Connection -> IO c -> IO c
> rollbackOnError conn =
>     bracketOnError (return())
>                    (const $ runSql conn "rollback" []) . const

================================================================================

= database read shortcuts

> readTurnPhase :: Connection -> IO String
> readTurnPhase conn = undefined
>   {-selectValue conn "select turn_phase from turn_phase_table"-}

> readCurrentWizard :: Connection -> IO String
> readCurrentWizard conn = undefined
>   {-selectValue conn
>          "select current_wizard from current_wizard_table"-}

> assertSelectedPiece :: Connection -> String -> String -> IO()
> assertSelectedPiece conn ptype allegiance = undefined {-do
>   t <- selectTuple conn "select ptype, allegiance from selected_piece"
>   assertEqual "selected piece"
>               (ptype,allegiance)
>               (lk "ptype" t, lk "allegiance" t)-}

> checkCurrentWizardPhase :: Connection -> String -> String -> IO()
> checkCurrentWizardPhase conn wiz phase = do
>   wiz' <- readCurrentWizard conn
>   phase' <- readTurnPhase conn
>   assertEqual "current wizard" wiz wiz'
>   assertEqual "current phase" phase' phase

> checkPieceDoneSelection :: Connection -> String -> String -> IO ()
> checkPieceDoneSelection conn ptype allegiance = undefined {-do
>     checkNoSelectedPiece conn
>     v <- selectTuples conn
>                       "select * from pieces_to_move\n\
>                       \where ptype=? and\n\
>                       \  allegiance=?" [ptype,allegiance]
>     assertBool "piece not in ptm" (null v)-}

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
> keyChooseSpell spellName = undefined {-safeLookup
>                              "get key for spell" spellName
>                              lookupChooseSpellKeys-}

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

===============================================================================

> startNewGame :: Connection -> IO ()
> startNewGame conn = do
>   newGame conn
>   resetCurrentEffects conn

> resetNewGameWidgetState conn = callSp conn "select action_reset_new_game_widget_state();" []

> setAllHuman conn = run conn "update new_game_widget_state set state='human';" []

> newGame conn = callSp conn "select action_client_new_game_using_new_game_widget_state" []

> resetCurrentEffects conn = callSp conn "select action_reset_current_effects" []

> callSp :: IConnection conn => conn -> String -> [String] -> IO ()
> callSp conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> runSql :: IConnection conn => conn -> String -> [String] -> IO ()
> runSql conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> dbAction :: IConnection conn => conn -> String -> [String] -> IO ()
> dbAction conn a args = callSp conn ("select action_" ++ a ++ "();") args