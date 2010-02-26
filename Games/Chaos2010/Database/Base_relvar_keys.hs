{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Base_relvar_keys where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Base_relvar_keys =
     Record
       (HCons (LVPair Constraint_name (Expr (Maybe String)))
          (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil))
 
base_relvar_keys :: Table Base_relvar_keys
base_relvar_keys = baseTable "base_relvar_keys"