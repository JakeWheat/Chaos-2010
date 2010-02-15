{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Operators where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Operators =
    Record (HCons (LVPair Operator_name (Expr (Maybe String))) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
operators :: Table Operators
operators = baseTable "operators"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Operator_name Field
---------------------------------------------------------------------------

data Operator_nameTag
type Operator_name = Proxy Operator_nameTag
instance ShowLabel Operator_name where
    showLabel _ = "operator_name"

operator_name :: Operator_name
operator_name = proxy