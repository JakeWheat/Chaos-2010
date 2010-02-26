{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Base_relvar_metadata where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Base_relvar_metadata =
     Record
       (HCons (LVPair Relvar_name (Expr String))
          (HCons (LVPair Type (Expr String)) HNil))
 
base_relvar_metadata :: Table Base_relvar_metadata
base_relvar_metadata = baseTable "base_relvar_metadata"
 
data TypeTag
 
type Type = Proxy TypeTag
 
instance ShowLabel Type where
        showLabel _ = "type"