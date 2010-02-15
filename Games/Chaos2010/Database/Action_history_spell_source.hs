{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Action_history_spell_source where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Action_history_spell_source =
    Record (HCons (LVPair Id (Expr (Maybe Int)))
            (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
              (HCons (LVPair Spell_name (Expr (Maybe String)))
               (HCons (LVPair X (Expr (Maybe Int)))
                (HCons (LVPair Y (Expr (Maybe Int))) HNil))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
action_history_spell_source :: Table Action_history_spell_source
action_history_spell_source = baseTable "action_history_spell_source"

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
-- Allegiance Field
---------------------------------------------------------------------------

data AllegianceTag
type Allegiance = Proxy AllegianceTag
instance ShowLabel Allegiance where showLabel _ = "allegiance"

allegiance :: Allegiance
allegiance = proxy

---------------------------------------------------------------------------
-- Spell_name Field
---------------------------------------------------------------------------

data Spell_nameTag
type Spell_name = Proxy Spell_nameTag
instance ShowLabel Spell_name where showLabel _ = "spell_name"

spell_name :: Spell_name
spell_name = proxy

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