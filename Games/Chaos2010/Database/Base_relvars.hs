{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Base_relvars where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Base_relvars =
    Record (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
base_relvars :: Table Base_relvars
base_relvars = baseTable "base_relvars"

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
