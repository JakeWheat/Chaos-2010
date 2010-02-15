{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Wizard_spell_choices_mr where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Wizard_spell_choices_mr =
    Record (HCons (LVPair Wizard_name (Expr String))
            (HCons (LVPair Spell_name (Expr String))
             (HCons (LVPair Imaginary (Expr (Maybe Bool))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
wizard_spell_choices_mr :: Table Wizard_spell_choices_mr
wizard_spell_choices_mr = baseTable "wizard_spell_choices_mr"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Wizard_name Field
---------------------------------------------------------------------------

data Wizard_nameTag
type Wizard_name = Proxy Wizard_nameTag
instance ShowLabel Wizard_name where showLabel _ = "wizard_name"

wizard_name :: Wizard_name
wizard_name = proxy

---------------------------------------------------------------------------
-- Spell_name Field
---------------------------------------------------------------------------

data Spell_nameTag
type Spell_name = Proxy Spell_nameTag
instance ShowLabel Spell_name where showLabel _ = "spell_name"

spell_name :: Spell_name
spell_name = proxy

---------------------------------------------------------------------------
-- Imaginary Field
---------------------------------------------------------------------------

data ImaginaryTag
type Imaginary = Proxy ImaginaryTag
instance ShowLabel Imaginary where showLabel _ = "imaginary"

imaginary :: Imaginary
imaginary = proxy
