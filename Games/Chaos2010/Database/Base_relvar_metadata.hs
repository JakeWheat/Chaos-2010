{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Base_relvar_metadata where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Base_relvar_metadata =
    Record (HCons (LVPair Relvar_name (Expr String))
            (HCons (LVPair Type (Expr String)) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
base_relvar_metadata :: Table Base_relvar_metadata
base_relvar_metadata = baseTable "base_relvar_metadata"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Relvar_name Field
---------------------------------------------------------------------------

data Relvar_nameTag
type Relvar_name = Proxy Relvar_nameTag
instance ShowLabel Relvar_name where showLabel _ = "relvar_name"

relvar_name :: Relvar_name
relvar_name = proxy

---------------------------------------------------------------------------
-- Type Field
---------------------------------------------------------------------------

data TypeTag
type Type = Proxy TypeTag
instance ShowLabel Type where showLabel _ = "type"

xtype :: Type
xtype = proxy