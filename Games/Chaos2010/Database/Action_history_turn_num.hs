{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Action_history_turn_num where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Action_history_turn_num =
    Record (HCons (LVPair Id (Expr (Maybe Int)))
            (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Turn_number (Expr (Maybe Int))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
action_history_turn_num :: Table Action_history_turn_num
action_history_turn_num = baseTable "action_history_turn_num"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Id Field
---------------------------------------------------------------------------

data IdTag
type Id = Proxy IdTag
instance ShowLabel Id where showLabel _ = "id"

xid :: Id
xid = proxy

---------------------------------------------------------------------------
-- History_name Field
---------------------------------------------------------------------------

data History_nameTag
type History_name = Proxy History_nameTag
instance ShowLabel History_name where showLabel _ = "history_name"

history_name :: History_name
history_name = proxy

---------------------------------------------------------------------------
-- Turn_number Field
---------------------------------------------------------------------------

data Turn_numberTag
type Turn_number = Proxy Turn_numberTag
instance ShowLabel Turn_number where showLabel _ = "turn_number"

turn_number :: Turn_number
turn_number = proxy