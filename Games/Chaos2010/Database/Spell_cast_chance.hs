{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Spell_cast_chance where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Spell_cast_chance =
    Record (HCons (LVPair Spell_name (Expr (Maybe String)))
            (HCons (LVPair Chance (Expr (Maybe Int))) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
spell_cast_chance :: Table Spell_cast_chance
spell_cast_chance = baseTable "spell_cast_chance"

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
-- Chance Field
---------------------------------------------------------------------------

data ChanceTag
type Chance = Proxy ChanceTag
instance ShowLabel Chance where showLabel _ = "chance"

chance :: Chance
chance = proxy