{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Prefered_targets where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Prefered_targets =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String)))
              (HCons (LVPair Preference (Expr (Maybe Int))) HNil))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
prefered_targets :: Table Prefered_targets
prefered_targets = baseTable "prefered_targets"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- X Field
---------------------------------------------------------------------------

data XTag
type X = Proxy XTag
instance ShowLabel X where showLabel _ = "x"

x :: X
x = proxy

---------------------------------------------------------------------------
-- Y Field
---------------------------------------------------------------------------

data YTag
type Y = Proxy YTag
instance ShowLabel Y where showLabel _ = "y"

y :: Y
y = proxy

---------------------------------------------------------------------------
-- Action Field
---------------------------------------------------------------------------

data ActionTag
type Action = Proxy ActionTag
instance ShowLabel Action where showLabel _ = "action"

action :: Action
action = proxy

---------------------------------------------------------------------------
-- Preference Field
---------------------------------------------------------------------------

data PreferenceTag
type Preference = Proxy PreferenceTag
instance ShowLabel Preference where showLabel _ = "preference"

preference :: Preference
preference = proxy