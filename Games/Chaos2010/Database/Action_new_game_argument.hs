{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Action_new_game_argument where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Action_new_game_argument =
    Record (HCons (LVPair Place (Expr Int))
            (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Computer_controlled (Expr Bool)) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
action_new_game_argument :: Table Action_new_game_argument
action_new_game_argument = baseTable "action_new_game_argument"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Place Field
---------------------------------------------------------------------------

data PlaceTag
type Place = Proxy PlaceTag
instance ShowLabel Place where showLabel _ = "place"

place :: Place
place = proxy

---------------------------------------------------------------------------
-- Wizard_name Field
---------------------------------------------------------------------------

data Wizard_nameTag
type Wizard_name = Proxy Wizard_nameTag
instance ShowLabel Wizard_name where showLabel _ = "wizard_name"

wizard_name :: Wizard_name
wizard_name = proxy

---------------------------------------------------------------------------
-- Computer_controlled Field
---------------------------------------------------------------------------

data Computer_controlledTag
type Computer_controlled = Proxy Computer_controlledTag
instance ShowLabel Computer_controlled where
    showLabel _ = "computer_controlled"

computer_controlled :: Computer_controlled
computer_controlled = proxy