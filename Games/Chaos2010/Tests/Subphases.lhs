
> module Games.Chaos2010.Tests.Subphases (subphases) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
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
> import Games.Chaos2010.Tests.TestUtils
>
> subphases db conn = testGroup "subphases" $
>                   map (\x -> x db conn)
>                         [testWalkCancelAttackDone
>                         ,testCancelMotionDone
>                         ,testWalkNoAvailAttackDone
>                         ,testWAttackDone
>                         ,testWalk2Done
>                         ,testWalk1CancelDone
>                         ,testFlyAttackDone
>                         ,testFAttackDone
>                         ,testCancelFlyDone
>                         ,testFlyNoAvailAttackDone
>                         ,testWalkAttackRangedDone
>                         ,testWalkAttackCancelDone
>                         ,testWalkCancelRangedDone
>                         ,testCancelRangedDone
>                         ,testWalkNoAvailAttackRangedDone
>                         ,testWalkStraightRangedDone
>                         ,testStraightRangedDone
>                         ,testAttackDone
>                         ,testNoAvailAttackDone
>                         ]


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

