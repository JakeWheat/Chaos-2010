{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.History_no_visuals where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type History_no_visuals =
    Record (HCons (LVPair History_name (Expr String)) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
history_no_visuals :: Table History_no_visuals
history_no_visuals = baseTable "history_no_visuals"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- History_name Field
---------------------------------------------------------------------------

data History_nameTag
type History_name = Proxy History_nameTag
instance ShowLabel History_name where showLabel _ = "history_name"

history_name :: History_name
history_name = proxy