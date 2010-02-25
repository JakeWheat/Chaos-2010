
> module Games.Chaos2010.Tests.Casting (casting) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Utils
> import Games.Chaos2010.Database.World_alignment_table
> import qualified Games.Chaos2010.Database.Monster_pieces as Mp
> import Games.Chaos2010.Tests.SetupGameState

> casting :: IConnection conn => Database -> conn -> Test.Framework.Test
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

> testCastGoblin :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastGoblin db = tctor "testCastGoblin" $ \conn -> do

setup the game, get to cast phase with the first wizard having
chosen goblin

>   setupGame db conn defaultGameState

>   addSpell1 conn "Buddha" "goblin"
>   sendKeyPress conn $ keyChooseSpell "goblin"
>   --get the next wizard to select disbelieve so we can check the
>   --auto next phase works
>   sendKeyPress conn "space"
>   sendKeyPress conn $ keyChooseSpell "disbelieve"
>   skipToPhase db conn "cast"

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
>   goSquare db conn 1 0
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
>   assertCurrentWizardPhase db "Kong Fuzi" "cast"

> testFailCastGoblin :: IConnection conn => Database -> conn -> Test.Framework.Test
> testFailCastGoblin db = tctor "testFailCastGoblin" $ \conn -> do
>   newGameReadyToCast db conn "goblin" (Just False) defaultGameState
>   rigActionSuccess conn "cast" False
>   goSquare db conn 1 0
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

> testCastMagicWood :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastMagicWood db = tctor "testCastMagicWood" $ \conn -> do
>   newGameReadyToCast db conn "magic_wood" Nothing defaultGameState
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

> testCastShadowWood :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastShadowWood db = tctor "testCastShadowWood" $ \conn -> do
>   newGameReadyToCast db conn "shadow_wood" Nothing defaultGameState
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
>   mapM_ (uncurry $ goSquare db conn)
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

> testCastMagicBolt :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastMagicBolt db = tctor "testCastMagicBolt" $ \conn -> do
>   newGameReadyToCast db conn "magic_bolt" Nothing defaultGameState
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
>   goSquare db conn 0 4
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

> testCastMagicBoltResisted :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastMagicBoltResisted db = tctor "testCastMagicBoltResisted" $ \conn -> do
>   newGameReadyToCast db conn "magic_bolt" Nothing defaultGameState
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
>   goSquare db conn 0 4
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

> testCastVengeanceWizard :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastVengeanceWizard db = tctor "testCastVengeanceWizard" $ \conn -> do
>   newGameReadyToCast1 db conn id "vengeance" Nothing
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
>   goSquare db conn 7 0
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

> testCastVengeanceMonster :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastVengeanceMonster db = tctor "testCastVengeanceMonster" $ \conn -> do
>   newGameReadyToCast1 db conn id "vengeance" Nothing
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
>   goSquare db conn 1 0
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

> testCastVengeanceMonsterResisted :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastVengeanceMonsterResisted db =
>     tctor "testCastVengeanceMonsterResisted" $ \conn -> do
>   newGameReadyToCast1 db conn id "vengeance" Nothing
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
>   goSquare db conn 1 0
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

> testCastSubversion :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastSubversion db = tctor "testCastSubversion" $ \conn -> do
>   newGameReadyToCast1 db conn id "subversion" Nothing
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
>   goSquare db conn 1 0
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

> testCastSubversionResisted :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastSubversionResisted db = tctor "testCastSubversionResisted" $ \conn -> do
>   newGameReadyToCast1 db conn id "subversion" Nothing
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
>   goSquare db conn 1 0
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

> testCastDisbelieveReal :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastDisbelieveReal db = tctor "testCastDisbelieveReal" $ \conn -> do
>   newGameReadyToCast1 db conn id "disbelieve" Nothing
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
>   goSquare db conn 1 0
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

> testCastDisbelieveImaginary :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastDisbelieveImaginary db = tctor "testCastDisbelieveImaginary" $ \conn -> do
>   newGameReadyToCast1 db conn id "disbelieve" Nothing
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
>   goSquare db conn 1 0
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

> testCastRaiseDead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastRaiseDead db = tctor "testCastRaiseDead" $ \conn -> do
>   newGameReadyToCast1 db conn id "raise_dead" Nothing
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
>   goSquare db conn 1 0
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


> testCastLaw :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastLaw db = tctor "testCastLaw" $ \conn -> do
>   newGameReadyToCast db conn "law" Nothing defaultGameState
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   align <- getWorldAlignment db
>   assertEqual "world alignment not law after casting law" 1 align

> getWorldAlignment :: Database -> IO Int
> getWorldAlignment db =
>   querySingleValue db (table world_alignment_table)

> testImaginary :: IConnection conn => Database -> conn -> Test.Framework.Test
> testImaginary db = tctor "testImaginary" $ \conn -> do

>   let setStuffUp1 = do
>                     setupGame db conn defaultGameState
>                     addSpell1 conn "Buddha" "goblin"
>                     sendKeyPress conn $ keyChooseSpell "goblin"
>   let setStuffUp2 = do
>                     skipToPhase db conn "cast"
>                     rigActionSuccess conn "cast" True
>                     goSquare db conn 1 0
>   let sv c = let t = do
>                       t1 <- table Mp.monster_pieces
>                       restrict ((t1 .!. Mp.x) .==. constJust 1)
>                       restrict ((t1 .!. Mp.y) .==. constJust 0)
>                       project $ copy Mp.imaginary t1
>                                   .*. emptyRecord
>              in querySingleValue db t >>= c . mb
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
