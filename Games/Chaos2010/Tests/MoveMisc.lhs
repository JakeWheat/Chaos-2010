
> module Games.Chaos2010.Tests.MoveMisc (moveMisc) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.Database.Pieces_mr
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.HaskellDBUtils
> import Games.Chaos2010.Database.Fields
> import Games.Chaos2010.Database.Valid_target_actions
>
> moveMisc :: IConnection conn => Database -> conn -> Test.Framework.Test
> moveMisc db conn = testGroup "moveMisc" $
>                   map (\xx -> xx db conn)
>                           [testAttackWizard
>                           ,testFlyAttack
>                           ,testMountThenMoveMount
>                           ,testMoveWhenMounted
>                           ,testDismount
>                           ,testMoveWhenAlreadyMounted
>                           ,testEnter --
>                           ,testExit --
>                           ,testAttackShadowForm
>                           ,testBlobSelection
>                           ,testAttackNonUndeadOnUndead --
>                           ,testMagicWeaponOnUndead --
>                           ,testAttackUndeadOnUndead --
>                           ,testNoMoveEngaged --
>                           ,testBreakEngaged --
>                           ]

== other move action tests

> testAttackWizard :: IConnection conn => Database -> conn -> Test.Framework.Test
> testAttackWizard db = tctor "testAttackWizard" $
>                           \conn -> do
>   setupGame db conn [setPhase "move"
>                     ,setCurrentWizard "Kong Fuzi"
>                     ,useBoard ("\n\
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
>                    ('H', [makePD "goblin" "Buddha"])]))]
>   selectAt db conn 1 0
>   rigActionSuccess conn "attack" True
>   attack db conn 0 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                    ('E', [makePD "eagle" "Buddha"])]))]
>   selectAt db conn 2 0
>   rigActionSuccess conn "attack" True
>   attack db conn 3 3
>   assertPiecesEquals db ("\n\
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
>                                 ('M', [makePD "wizard" "Buddha",
>                                        makePD "pegasus" "Buddha"])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1P     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)]
>   selectAt db conn 0 0
>   assertSelectedPiece db "wizard" "Buddha"
>   walk db conn 1 0
>   selectAt db conn 1 0
>   assertSelectedPiece db "pegasus" "Buddha"
>   fly db conn 2 2
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                           makePD "pegasus" "Buddha"])])]
>   --select then cancel the wizard
>   selectAt db conn 1 0
>   assertSelectedPiece db "wizard" "Buddha"
>   cancel db conn
>   --now we can select the monster
>   selectAt db conn 1 0
>   assertSelectedPiece db "pegasus" "Buddha"
>   fly db conn 2 2
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                           makePD "pegasus" "Buddha"])])]
>   selectAt db conn 1 0
>   walk db conn 1 1
>   selectAt db conn 1 0
>   fly db conn 3 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                           makePD "pegasus" "Buddha"])])]
>   selectAt db conn 1 0
>   cancel db conn
>   selectAt db conn 1 0
>   fly db conn 3 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                           makePD "dark_citadel" "Buddha"])])]
>   selectAt db conn 1 0
>   assertSelectedPiece db "wizard" "Buddha"
>   walk db conn 1 1
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
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
>                   [('P', [makePD "dark_citadel" "Buddha"])]))]
>   selectAt db conn 0 0
>   walk db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "vengeance" ++
>                  [useBoard ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))]
>   oldStats <- getStats
>   --actionGo db conn
>   skipToPhase db conn "move"
>   selectAt db conn 0 0
>   rigActionSuccess conn "attack" False
>   attack db conn 1 0
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
>            [('G', [makeFPD "ghost" "Kong Fuzi" False True]),
>             ('S', [makeFPD "goblin" "Buddha" False True])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   selectAt db conn 1 0
>   rigActionSuccess conn "attack" True
>   attack db conn 1 1
>   assertPiecesEquals db ("\n\
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
>            [('G', [makeFPD "ghost" "Kong Fuzi" False True]),
>             ('S', [makeFPD "goblin" "Buddha" False False])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   selectAt db conn 1 0
>   assertRelvarValue db (do
>                         t1 <- table valid_target_actions
>                         restrict $ t1 # x .==. constJust 1
>                         restrict $ t1 # y .==. constJust 1
>                         project $ copyAll t1
>                        ) []

> testMagicWeaponOnUndead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testMagicWeaponOnUndead db = tctor "testMagicWeaponOnUndead" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [makeFPD "ghost" "Kong Fuzi" False True]),
>             ('S', [makeFPD "spectre" "Buddha" False True])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1S     2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   addMagicSword db conn "Buddha"
>   --printWiz
>   --querySelectedPiece db >>= print
>   selectAt db conn 0 0
>   --querySelectedPiece db >>= print
>   rigActionSuccess conn "attack" True
>   attack db conn 1 1
>   --querySelectedPiece db >>= print
>   assertPiecesEquals db ("\n\
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

> testNoMoveEngaged :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNoMoveEngaged db = tctor "testNoMoveEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [makeFPD "goblin" "Kong Fuzi" False True])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1      2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   rigActionSuccess conn "break_engaged" False
>   selectAt db conn 0 0
>   assertRelvarValue db (do
>                         t1 <- table valid_target_actions
>                         restrict $ t1 # x .==. constJust 0
>                         restrict $ t1 # y .==. constJust 1
>                         project $ copyAll t1
>                        ) []

> testBreakEngaged :: IConnection conn => Database -> conn -> Test.Framework.Test
> testBreakEngaged db = tctor "testBreakEngaged" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('G', [makeFPD "goblin" "Kong Fuzi" False True])]
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1      2      3\n\
>                   \ G             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   rigActionSuccess conn "break_engaged" True
>   selectAt db conn 0 0
>   walk db conn 0 1
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn [setPhase "move"
>                     ,useBoard ("\n\
>                   \1O     2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)]
>   --selectAt db conn 1 0
>   assertRelvarValue db (do
>                         t1 <- table valid_target_actions
>                         restrict $ t1 # x .==. constJust 1
>                         restrict $ t1 # y .==. constJust 0
>                         project $ copyAll t1
>                        ) []
>   --assertNoSelectedPiece db
>   selectAt db conn 0 0
>   rigActionSuccess conn "attack" True
>   attack db conn 1 0
>   assertNoSelectedPiece db
>   assertPiecesEquals db ("\n\
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
>   selectAt db conn 1 0
>   assertSelectedPiece db "goblin" "Buddha"


check attack when has mount moves wizard
check attack,nter,mount from mount
