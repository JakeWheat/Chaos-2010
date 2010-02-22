
> module Games.Chaos2010.Tests.Upgrades (upgrades) where

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
> upgrades db conn = testGroup "upgrades" $
>                   map (\x -> x db conn)
>                   [testCastShield
>                   ,testCastArmour
>                   ,testCastKnife
>                   ,testCastSword
>                   ,testCastBow
>                   ,testCastWings
>                   ,testCastShadowForm]

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


todo:
stack upgrades - different spells, same spells e.g. magic knife
twice, magic knife then magic sword

if there is a way that a wizard can lose an upgrade that needs to be
tested too

