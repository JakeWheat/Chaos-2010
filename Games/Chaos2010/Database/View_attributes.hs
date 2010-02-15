{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.View_attributes where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type View_attributes =
    Record (HCons (LVPair Attribute_name (Expr (Maybe String)))
            (HCons (LVPair Type_name (Expr (Maybe String)))
             (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
view_attributes :: Table View_attributes
view_attributes = baseTable "view_attributes"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Attribute_name Field
---------------------------------------------------------------------------

data Attribute_nameTag
type Attribute_name = Proxy Attribute_nameTag
instance ShowLabel Attribute_name where
    showLabel _ = "attribute_name"

attribute_name :: Attribute_name
attribute_name = proxy

---------------------------------------------------------------------------
-- Type_name Field
---------------------------------------------------------------------------

data Type_nameTag
type Type_name = Proxy Type_nameTag
instance ShowLabel Type_name where showLabel _ = "type_name"

type_name :: Type_name
type_name = proxy

---------------------------------------------------------------------------
-- Relvar_name Field
---------------------------------------------------------------------------

data Relvar_nameTag
type Relvar_name = Proxy Relvar_nameTag
instance ShowLabel Relvar_name where showLabel _ = "relvar_name"

relvar_name :: Relvar_name
relvar_name = proxy