
> module Games.Chaos2010.Tests.MoveMisc (moveMisc) where

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
> moveMisc db conn = testGroup "moveMisc" $
>                   map (\x -> x db conn)
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
