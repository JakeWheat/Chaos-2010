{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Valid_activate_actions where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Valid_activate_actions =
    Record (HCons (LVPair Action (Expr (Maybe String))) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
valid_activate_actions :: Table Valid_activate_actions
valid_activate_actions = baseTable "valid_activate_actions"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Action Field
---------------------------------------------------------------------------

data ActionTag
type Action = Proxy ActionTag
instance ShowLabel Action where showLabel _ = "action"

action :: Action
action = proxy
