{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Test_action_overrides where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Test_action_overrides =
     Record
       (HCons (LVPair Override (Expr String))
          (HCons (LVPair Setting (Expr Bool)) HNil))
 
test_action_overrides :: Table Test_action_overrides
test_action_overrides = baseTable "test_action_overrides"