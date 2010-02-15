{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.New_game_widget_state where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type New_game_widget_state =
    Record (HCons (LVPair Line (Expr Int))
            (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Sprite (Expr String))
              (HCons (LVPair Colour (Expr String))
               (HCons (LVPair State (Expr String)) HNil)))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
new_game_widget_state :: Table New_game_widget_state
new_game_widget_state = baseTable "new_game_widget_state"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Line Field
---------------------------------------------------------------------------

data LineTag
type Line = Proxy LineTag
instance ShowLabel Line where showLabel _ = "line"

line :: Line
line = proxy

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

---------------------------------------------------------------------------
-- State Field
---------------------------------------------------------------------------

data StateTag
type State = Proxy StateTag
instance ShowLabel State where showLabel _ = "state"

state :: State
state = proxy