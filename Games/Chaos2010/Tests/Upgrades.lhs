

> {-# LANGUAGE FlexibleContexts,TemplateHaskell #-}
> {-# OPTIONS_GHC -fcontext-stack57 #-}
> module Games.Chaos2010.Tests.Upgrades (upgrades) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB hiding (insert, update)
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.ThHdb

> import Games.Chaos2010.Database.Pieces_mr
> import Games.Chaos2010.Database.Fields

> $(makeValueTypes [[t|Pieces_mr|]])

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

> addStat :: (Num a,
>             Functor f,
>             HasField l r (f a),
>             HUpdateAtHNat n (LVPair l (f a)) r r,
>             HFind l ls n,
>             RecordLabels r ls) =>
>            l -> a -> Record r -> Record r
> addStat l n hl = update l (fmap (+n)) hl

> update :: (HasField l r a,
>            HUpdateAtHNat n (LVPair l a) r r,
>            HFind l ls n,
>            RecordLabels r ls) =>
>           l -> (a -> a) -> Record r -> Record r
> update l f hl =
>   let v = hl # l
>   in (l .=. f v) .@. hl

> doUpgradeTest :: IConnection conn =>
>                  conn
>               -> Database
>               -> String
>               -> (Pieces_mr_v -> Pieces_mr_v)
>               -> IO ()
> doUpgradeTest conn db spell attrChange = do
>   setupGame db conn $ readyToCast spell
>   oldStats <- getStats
>   rigActionSuccess conn "cast" True
>   castActivateSpell db conn -- sendKeyPress conn "Return"
>   newStats <- getStats
>   assertEqual "upgraded stats" (attrChange oldStats) newStats
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
>   where
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
