{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Base_relvar_attributes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Base_relvar_attributes =
     Record
       (HCons (LVPair Attribute_name (Expr (Maybe String)))
          (HCons (LVPair Type_name (Expr (Maybe String)))
             (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil)))
 
base_relvar_attributes :: Table Base_relvar_attributes
base_relvar_attributes = baseTable "base_relvar_attributes"