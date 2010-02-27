
> module Games.Chaos2010.Tests.Subphases (subphases) where

> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.DBUpdates

>
> subphases :: IConnection conn => Database -> conn -> Test.Framework.Test
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

> testWalkCancelAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkCancelAttackDone db = tctor "testWalkCancelAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G R   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     --select goblin
>     goSquare db conn 1 0
>     assertSelectedPiece db "goblin" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertPiecesEquals db ("\n\
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
>     assertMoveSubphase db "attack"
>     cancel db conn
>     assertPieceDoneSelection db "goblin" "Buddha"

> testCancelMotionDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCancelMotionDone db = tctor "testCancelMotionDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "goblin" "Buddha"
>     assertMoveSubphase db "motion"
>     cancel db conn
>     assertPieceDoneSelection db "goblin" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalkNoAvailAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkNoAvailAttackDone db = tctor "testWalkNoAvailAttackDone" $
>                                    \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "goblin" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertPieceDoneSelection db "goblin" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWAttackDone db = tctor "testWAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "goblin" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('E', [makePD "goblin" "Buddha",
>                       makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "goblin" "Buddha"
>     assertMoveSubphase db "motion"
>     rigActionSuccess conn "attack" True
>     goSquare db conn 2 0
>     assertPieceDoneSelection db "goblin" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalk2Done :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalk2Done db = tctor "testWalk2Done" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [makePD "bear" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1B   R 2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "bear" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertSelectedPiece db "bear" "Buddha"
>     assertMoveSubphase db "motion"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 0
>     assertPieceDoneSelection db "bear" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalk1CancelDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalk1CancelDone db = tctor "testWalk1CancelDone" $
>                              \conn -> do
>     let pl = (wizardPiecesList ++
>               [('B', [makePD "bear" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1B R   2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "bear" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertSelectedPiece db "bear" "Buddha"
>     assertMoveSubphase db "motion"
>     assertPiecesEquals db ("\n\
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
>     cancel db conn
>     assertPieceDoneSelection db "bear" "Buddha"

> testFlyAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testFlyAttackDone db = tctor "testFlyAttackDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "eagle" "Buddha",
>                       makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "eagle" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 3 0
>     assertPiecesEquals db ("\n\
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
>     assertMoveSubphase db "attack"
>     rigActionSuccess conn "attack" True
>     goSquare db conn 4 0
>     assertPieceDoneSelection db "eagle" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testFAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testFAttackDone db = tctor "testFAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G  R  2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "eagle" "Buddha"
>     assertMoveSubphase db "motion"
>     rigActionSuccess conn "attack" False
>     goSquare db conn 4 0
>     assertPieceDoneSelection db "eagle" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testCancelFlyDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCancelFlyDone db = tctor "testCancelFlyDone" $
>                            \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1GR    2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "eagle" "Buddha"
>     assertMoveSubphase db "motion"
>     cancel db conn
>     assertPieceDoneSelection db "eagle" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testFlyNoAvailAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testFlyNoAvailAttackDone db = tctor "testFlyNoAvailAttackDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "eagle" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                        ,useBoard ("\n\
>                   \1G    R2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "eagle" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 3 0
>     assertPieceDoneSelection db "eagle" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalkAttackRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkAttackRangedDone db = tctor "testWalkAttackRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "elf" "Buddha",
>                       makePD "giant_rat" "dead"]),
>                ('r', [makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertMoveSubphase db "attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 0
>     assertMoveSubphase db "ranged_attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 1
>     assertPiecesEquals db ("\n\
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
>     assertPieceDoneSelection db "elf" "Buddha"

> testWalkAttackCancelDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkAttackCancelDone db = tctor "testWalkAttackCancelDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('H', [makePD "elf" "Buddha",
>                       makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertMoveSubphase db "attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 0
>     assertMoveSubphase db "ranged_attack"
>     assertPiecesEquals db ("\n\
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
>     cancel db conn
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalkCancelRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkCancelRangedDone db = tctor "testWalkCancelRangedDone" $
>                                   \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G R   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertMoveSubphase db "attack"
>     assertPiecesEquals db ("\n\
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
>     cancel db conn
>     assertMoveSubphase db "ranged_attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 1
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testCancelRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCancelRangedDone db = tctor "testCancelRangedDone" $
>                               \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('r', [makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1 GR   2      3\n\
>                   \   R           \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 2 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     cancel db conn
>     assertMoveSubphase db "ranged_attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 3 1
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalkNoAvailAttackRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkNoAvailAttackRangedDone db =
>   tctor "testWalkNoAvailAttackRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard("\n\
>                   \1G  R  2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertPiecesEquals db ("\n\
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
>     assertMoveSubphase db "ranged_attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 4 1
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testWalkStraightRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWalkStraightRangedDone db =
>   tctor "testWalkStraightRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G  R  2      3\n\
>                   \   RR          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     goSquare db conn 2 0
>     assertPiecesEquals db ("\n\
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
>     assertMoveSubphase db "attack"
>     assertPiecesEquals db ("\n\
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
>     goSquare db conn 4 1
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testStraightRangedDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testStraightRangedDone db =
>   tctor "testStraightRangedDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('G', [makePD "elf" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1G  R  2      3\n\
>                   \  R R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "elf" "Buddha"
>     assertMoveSubphase db "motion"
>     rigActionSuccess conn "ranged_attack" False
>     goSquare db conn 4 0
>     assertPiecesEquals db ("\n\
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
>     assertPieceDoneSelection db "elf" "Buddha"
>     assertPiecesEquals db ("\n\
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

> testAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackDone db = tctor "testAttackDone" $ \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [makePD "shadow_tree" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"]),
>                ('r', [makePD "giant_rat" "dead"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1WR    2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertSelectedPiece db "shadow_tree" "Buddha"
>     assertMoveSubphase db "attack"
>     rigActionSuccess conn "attack" True
>     goSquare db conn 2 0
>     assertPiecesEquals db ("\n\
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
>     assertPieceDoneSelection db "shadow_tree" "Buddha"

> testNoAvailAttackDone :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNoAvailAttackDone db = tctor "testNoAvailAttackDone" $
>                                \conn -> do
>     let pl = (wizardPiecesList ++
>               [('W', [makePD "shadow_tree" "Buddha"]),
>                ('R', [makePD "giant_rat" "Kong Fuzi"])])
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \1W     2      3\n\
>                   \    R          \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>     goSquare db conn 1 0
>     assertPieceDoneSelection db "shadow_tree" "Buddha"

