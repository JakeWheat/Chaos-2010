
> module Games.Chaos2010.Tests.Casting (casting) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Monster_pieces
> import Games.Chaos2010.Database.Fields
> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.HaskellDBUtils

> casting :: IConnection conn => Database -> conn -> Test.Framework.Test
> casting db conn = testGroup "casting" $
>                   map (\z -> z db conn)
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

>   setupGame db conn [addSpell "Buddha" "goblin"]
>   chooseSpell db conn "goblin"
>   --get the next wizard to select disbelieve so we can check the
>   --auto next phase works
>   nextPhase conn
>   chooseSpell db conn "disbelieve"
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "goblin"

>   --newGameReadyToCast db conn "goblin" (Just False) defaultGameState
>   rigActionSuccess conn "cast" False
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "magic_wood" ++
>                     [useBoard ("\n\
>                   \1      2      3\n\
>                   \ S             \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('S', [makePD "shadow_tree" "Buddha"])]))]
>   rigActionSuccess conn "cast" True
>   castActivateSpell db conn -- sendKeyPress conn "Return"
>   assertPiecesEquals db ("\n\
>                   \1  W W 2      3\n\
>                   \ S             \n\
>                   \   W W         \n\
>                   \W              \n\
>                   \4 W W         5\n\
>                   \W              \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('W', [makePD "magic_tree" "Buddha"])
>                   ,('S', [makePD "shadow_tree" "Buddha"])]))

> testCastShadowWood :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastShadowWood db = tctor "testCastShadowWood" $ \conn -> do
>   setupGame db conn $ readyToCast "shadow_wood"
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
>   mapM_ (uncurry $ castTargetSpell db conn)
>         [(1,0),(3,0),(5,0),(0,2),(2,2),(4,2),(1,4),(3,4)]
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "magic_bolt"
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
>   castTargetSpell db conn 0 4
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "magic_bolt"
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
>   castTargetSpell db conn 0 4
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "vengeance" ++
>                     [useBoard ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))]
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
>   castTargetSpell db conn 7 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "vengeance" ++
>                     [useBoard ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))]
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   tctor "testCastVengeanceMonsterResisted" $ \conn -> do
>   setupGame db conn $ readyToCast "vengeance" ++
>                     [useBoard ("\n\
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
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))]
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "subversion" ++
>                     [useBoard ("\n\
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "subversion" ++
>                     [useBoard ("\n\
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "disbelieve" ++
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "disbelieve" ++
>                     [useBoard ("\n\
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
>                   [('G', [makeFPD "goblin" "Kong Fuzi" True False])]))]
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>   setupGame db conn $ readyToCast "raise_dead" ++
>                     [useBoard ("\n\
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
>                   [('G', [makePD "goblin" "dead"])]))]
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
>   castTargetSpell db conn 1 0
>   assertPiecesEquals db ("\n\
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
>                   [('G', [makeFPD "goblin" "Buddha" False True])]))


> testCastLaw :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastLaw db = tctor "testCastLaw" $ \conn -> do
>   setupGame db conn $ readyToCast "law"
>   rigActionSuccess conn "cast" True
>   castActivateSpell db conn --sendKeyPress conn "Return"
>   align <- getWorldAlignment db
>   assertEqual "world alignment not law after casting law" 1 align

> getWorldAlignment :: Database -> IO Int
> getWorldAlignment db =
>   querySingleValue db (table world_alignment_table)

> testImaginary :: IConnection conn => Database -> conn -> Test.Framework.Test
> testImaginary db = tctor "testImaginary" $ \conn -> do

>   let setStuffUp1 = setupGame db conn $ addAndChoose "goblin"
>   let setStuffUp2 = do
>                     skipToPhase db conn "cast"
>                     rigActionSuccess conn "cast" True
>                     castTargetSpell db conn 1 0
>   let sv c = let t = do
>                       t1 <- table monster_pieces
>                       restrict ((t1 .!. x) .==. constJust 1)
>                       restrict ((t1 .!. y) .==. constJust 0)
>                       project $ copy imaginary t1
>                                   .*. emptyRecord
>              in querySingleValue db t >>= c . mb
>   setStuffUp1
>   setStuffUp2
>   sv (\v -> assertBool "default real for monsters" $ not v)

>   setStuffUp1
>   setSpellChoiceReal db conn
>   --sendKeyPress conn "n"
>   setStuffUp2
>   sv (\v -> assertBool "cast real monster" $ not v)

>   setStuffUp1
>   setSpellChoiceImaginary db conn
>   setStuffUp2
>   sv (\v -> assertBool "cast imaginary monster" v)

>   setStuffUp1
>   setSpellChoiceImaginary db conn
>   --sendKeyPress conn "y"
>   setSpellChoiceReal db conn
>   --sendKeyPress conn "n"
>   setStuffUp2
>   sv (\v -> assertBool "dither then cast real monster" $ not v)

>   setStuffUp1
>   setSpellChoiceReal db conn
>   --sendKeyPress conn "n"
>   setSpellChoiceImaginary db conn
>   --sendKeyPress conn "y"
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
