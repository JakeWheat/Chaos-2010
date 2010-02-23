
> module Games.Chaos2010.Tests.MoveMisc (moveMisc) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Database.Pieces_mr
> --import Games.Chaos2010.Database.Wizards
>
> moveMisc :: IConnection conn => Database -> conn -> Test.Framework.Test
> moveMisc db conn = testGroup "moveMisc" $
>                   map (\xx -> xx db conn)
>                          [testAttackWizard
>                          ,testFlyAttack
>                          ,testMountThenMoveMount
>                          ,testMoveWhenMounted
>                          ,testDismount
>                          ,testMoveWhenAlreadyMounted
>                          ,testEnter
>                          ,testExit
>                          ,testAttackShadowForm
>                          ,testBlobSelection
>                         ,testAttackNonUndeadOnUndead
>                         ,testMagicWeaponOnUndead
>                         ,testAttackUndeadOnUndead
>                         ,testNoMoveEngaged
>                         ,testBreakEngaged
>                        ]

== other move action tests

> testAttackWizard :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackWizard db = tctor "testAttackWizard" $
>                           \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 0 0
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

> testFlyAttack :: IConnection conn => Database -> conn -> Test.Framework.Test
> testFlyAttack db = tctor "testFlyAttack" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 2 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 3 3
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

> testMountThenMoveMount :: IConnection conn => Database -> conn -> Test.Framework.Test
> testMountThenMoveMount db = tctor "testMountThenMoveMount" $
>                              \conn -> do
>   let pl = wizardPiecesList ++ [('P', [makePD "pegasus" "Buddha"]),
>             ('M', [makePD "wizard" "Buddha",
>                    makePD "pegasus" "Buddha"])]
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 0 0
>   assertSelectedPiece db "wizard" "Buddha"
>   goSquare db conn 1 0
>   sendKeyPress conn "Return"
>   assertSelectedPiece db "pegasus" "Buddha"
>   goSquare db conn 2 2
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

> testMoveWhenMounted :: IConnection conn => Database -> conn -> Test.Framework.Test
> testMoveWhenMounted db = tctor "testMoveWhenMounted" $
>                              \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   assertSelectedPiece db "wizard" "Buddha"
>   sendKeyPress conn "End"
>   --now we can select the monster
>   sendKeyPress conn "Return"
>   assertSelectedPiece db "pegasus" "Buddha"
>   goSquare db conn 2 2
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

> testDismount :: IConnection conn => Database -> conn -> Test.Framework.Test
> testDismount db = tctor "testDismount" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   goSquare db conn 1 1
>   goSquare db conn 1 0
>   goSquare db conn 3 0
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

> testMoveWhenAlreadyMounted :: IConnection conn => Database -> conn -> Test.Framework.Test
> testMoveWhenAlreadyMounted db = tctor "testMoveWhenAlreadyMounted" $
>                                     \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   sendKeyPress conn "End"
>   sendKeyPress conn "Return"
>   goSquare db conn 3 0
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

> testExit :: IConnection conn => Database -> conn -> Test.Framework.Test
> testExit db = tctor "testExit" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   assertSelectedPiece db "wizard" "Buddha"
>   goSquare db conn 1 1
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

> testEnter :: IConnection conn => Database -> conn -> Test.Framework.Test
> testEnter db = tctor "testEnter" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 0 0
>   goSquare db conn 1 0
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

> testAttackShadowForm :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackShadowForm db = tctor "testAttackShadowForm" $ \conn -> do
>   newGameWithBoardReadyToCast db conn "shadow_form"
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
>   oldStats <- getStats
>   sendKeyPress conn "Return"
>   skipToPhase db conn "move"
>   goSquare db conn 0 0
>   rigActionSuccess conn "attack" False
>   goSquare db conn 1 0
>   newStats <- getStats
>   assertEqual "attacking loses shadow form" oldStats newStats
>   where
>     getStats =
>       query db $ do
>         t1 <- table pieces_mr
>         restrict ((t1 .!. ptype) .==. constJust "wizard")
>         restrict ((t1 .!. allegiance) .==. constJust "Buddha")
>         project $ copyAll t1



> testAttackUndeadOnUndead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackUndeadOnUndead db = tctor "testAttackUndeadOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "goblin" "Buddha" Real Undead])]
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 1 1
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

> testAttackNonUndeadOnUndead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackNonUndeadOnUndead db = tctor "testAttackNonUndeadOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "goblin" "Buddha" Real Alive])]
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 1 1
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

> testMagicWeaponOnUndead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testMagicWeaponOnUndead db = tctor "testMagicWeaponOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "ghost" "Kong Fuzi" Real Undead]),
>             ('S', [PieceDescription "spectre" "Buddha" Real Undead])]
>   startNewGameReadyToMove db conn ("\n\
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
>   addMagicSword db "Buddha"
>   --printWiz
>   --querySelectedPiece db >>= print
>   goSquare db conn 0 0
>   --querySelectedPiece db >>= print
>   rigActionSuccess conn "attack" True
>   goSquare db conn 1 1
>   --querySelectedPiece db >>= print
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
>   {-where
>     printWiz = do
>                rel <- query db $ do
>                                  t1 <- table wizards
>                                  --restrict $ t1 .!. wizard_name .==. constant "Buddha"
>                                  project $ copy wizard_name t1
>                                            .*. copy magic_sword t1
>                                            .*. emptyRecord
>                print rel-}

== misc todo

> testNoMoveEngaged :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNoMoveEngaged db = tctor "testNoMoveEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "goblin" "Kong Fuzi" Real Undead])]
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 0 0
>   goSquare db conn 0 1
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

> testBreakEngaged :: IConnection conn => Database -> conn -> Test.Framework.Test
> testBreakEngaged db = tctor "testBreakEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [PieceDescription "goblin" "Kong Fuzi" Real Undead])]
>   startNewGameReadyToMove db conn ("\n\
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
>   goSquare db conn 0 0
>   goSquare db conn 0 1
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

> testBlobSelection :: IConnection conn => Database -> conn -> Test.Framework.Test
> testBlobSelection db = tctor "testBlobSelection" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "goblin" "Buddha",
>                    makePD "gooey_blob" "Kong Fuzi"]),
>             ('g', [makePD "goblin" "Buddha"])]
>   startNewGameReadyToMoveNoSpread db conn ("\n\
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
>   goSquare db conn 1 0
>   assertNoSelectedPiece db
>   goSquare db conn 0 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 1 0
>   assertNoSelectedPiece db
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
>   goSquare db conn 1 0
>   assertSelectedPiece db "goblin" "Buddha"


check attack when has mount moves wizard
check attack,nter,mount from mount
