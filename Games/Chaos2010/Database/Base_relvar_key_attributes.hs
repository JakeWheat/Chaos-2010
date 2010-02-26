{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Base_relvar_key_attributes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Base_relvar_key_attributes =
     Record
       (HCons (LVPair Constraint_name (Expr (Maybe String)))
          (HCons (LVPair Attribute_name (Expr (Maybe String))) HNil))
 
base_relvar_key_attributes :: Table Base_relvar_key_attributes
base_relvar_key_attributes = baseTable "base_relvar_key_attributes"