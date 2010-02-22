
> module Games.Chaos2010.Tests.Casting (casting) where

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
> casting db conn = testGroup "casting" $
>                   map (\x -> x db conn)
>                       [testCastGoblin
>                       ,testFailCastGoblin
>                       ,testCastMagicWood
>                       ,testCastShadowWood
>                       ,testCastMagicBolt
>                       ,testCastMagicBoltResisted
>                       ,testCastVengeanceWizard
>                       ,testCastVengeanceMonster
>                       ,testCastVengeanceMonsterResisted
>                       ,testCastSubversion
>                       ,testCastSubversionResisted
>                       ,testCastDisbelieveReal
>                       ,testCastDisbelieveImaginary
>                       ,testCastRaiseDead
>                       ,testCastLaw
>                       ,testImaginary]

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
