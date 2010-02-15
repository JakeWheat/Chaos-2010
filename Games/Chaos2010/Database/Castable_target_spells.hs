{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Castable_target_spells where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Castable_target_spells =
    Record (HCons (LVPair Spell_name (Expr (Maybe String)))
            (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
castable_target_spells :: Table Castable_target_spells
castable_target_spells = baseTable "castable_target_spells"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
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