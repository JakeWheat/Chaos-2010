

> {-# LANGUAGE FlexibleContexts #-}
> module Games.Chaos2010.Tests.Upgrades (upgrades) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB hiding (insert)
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Database.Pieces_mr as P

> upgrades :: IConnection conn => Database -> conn -> Test.Framework.Test
> upgrades db conn = testGroup "upgrades" $
>                   map (\xx -> xx db conn)
>                   [testCastShield
>                   ,testCastArmour
>                   ,testCastKnife
>                   ,testCastSword
>                   ,testCastBow
>                   ,testCastWings
>                   ,testCastShadowForm]

> testCastShield :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastShield db = tctor "testCastShield" $ \conn ->
>   doUpgradeTest conn db "magic_shield" $ addStat physical_defense 2


> testCastArmour :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastArmour db = tctor "testCastArmour" $ \conn ->
>   doUpgradeTest conn db "magic_armour" $
>                 addStat physical_defense 4

> testCastKnife :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastKnife db = tctor "testCastKnife" $ \conn ->
>   doUpgradeTest conn db "magic_knife" $
>                 addStat attack_strength 2

> testCastSword :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastSword db = tctor "testCastSword" $ \conn ->
>   doUpgradeTest conn db "magic_sword" $
>                 addStat attack_strength 4

> testCastBow :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastBow db = tctor "testCastBow" $ \conn ->
>   doUpgradeTest conn db "magic_bow" $
>                 setStat ranged_weapon_type "projectile" .
>                 setStat range 6 .
>                 setStat ranged_attack_strength 6

> testCastWings :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastWings db = tctor "testCastWings" $ \conn ->
>   doUpgradeTest conn db "magic_wings" $
>                 setStat speed 6 .
>                 setStat flying True

> testCastShadowForm :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastShadowForm db = tctor "testCastShadowForm" $ \conn ->
>   doUpgradeTest conn db "shadow_form" $
>                 addStat physical_defense 2 .
>                 addStat agility 2 .
>                 setStat speed 3

> setStat :: (HUpdateAtHNat n (LVPair t (Maybe a)) t2 l'
>            ,HFind t ls n
>            ,RecordLabels t2 ls) =>
>            t -> a -> Record t2 -> Record l'
> setStat f v r = (f .=. Just v) .@. r

> addStat :: (HasField l t2 (f a),
>             Num a,
>             Functor f,
>             HUpdateAtHNat n (LVPair l (f a)) t2 l',
>             HFind l ls n,
>             RecordLabels t2 ls) =>
>             l -> a -> Record t2 -> Record l'
> addStat f v r = let v1 = r # f
>                 in (f .=. (fmap (+v) v1)) .@. r

> {-doUpgradeTest :: IConnection conn =>
>                  conn
>               -> Database
>               -> String
>               -> (Pieces_mr -> Pieces_mr)
>               -> IO ()-}
> doUpgradeTest conn db spell attrChange = do
>   newGameReadyToCast db conn spell
>   oldStats <- getStats
>   rigActionSuccess conn "cast" True
>   sendKeyPress conn "Return"
>   newStats <- getStats
>   assertEqual "upgraded stats" (attrChange oldStats) newStats
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
>   where
>     --getStats :: IO Pieces_mr_tuple
>     --getStats :: IO Pieces_mr
>     getStats = do
>       rel <- query db $ do
>         t1 <- table pieces_mr
>         restrict ((t1 .!. ptype) .==. constJust "wizard")
>         restrict ((t1 .!. allegiance) .==. constJust "Buddha")
>         project $ copyAll t1
>       return $ head rel


todo:
stack upgrades - different spells, same spells e.g. magic knife
twice, magic knife then magic sword

if there is a way that a wizard can lose an upgrade that needs to be
tested too
