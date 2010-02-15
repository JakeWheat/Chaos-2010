{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Wizard_sprites where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Wizard_sprites =
    Record (HCons (LVPair Wizard_name (Expr (Maybe String)))
            (HCons (LVPair Sprite (Expr (Maybe String)))
             (HCons (LVPair Colour (Expr (Maybe String))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
wizard_sprites :: Table Wizard_sprites
wizard_sprites = baseTable "wizard_sprites"

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
-- Sprite Field
---------------------------------------------------------------------------

data SpriteTag
type Sprite = Proxy SpriteTag
instance ShowLabel Sprite where showLabel _ = "sprite"

sprite :: Sprite
sprite = proxy

---------------------------------------------------------------------------
-- Colour Field
---------------------------------------------------------------------------

data ColourTag
type Colour = Proxy ColourTag
instance ShowLabel Colour where showLabel _ = "colour"

colour :: Colour
colour = proxy