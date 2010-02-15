{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack51 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Board_sprites1_view where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Board_sprites1_view =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Ptype (Expr (Maybe String)))
              (HCons (LVPair Allegiance (Expr (Maybe String)))
               (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair Sprite (Expr (Maybe String)))
                 (HCons (LVPair Colour (Expr (Maybe String)))
                  (HCons (LVPair Sp (Expr (Maybe Int)))
                   (HCons (LVPair Start_tick (Expr (Maybe Int)))
                    (HCons (LVPair Animation_speed (Expr (Maybe Int)))
                     (HCons (LVPair Selected (Expr (Maybe Bool))) HNil)))))))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
board_sprites1_view :: Table Board_sprites1_view
board_sprites1_view = baseTable "board_sprites1_view"

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
-- Ptype Field
---------------------------------------------------------------------------

data PtypeTag
type Ptype = Proxy PtypeTag
instance ShowLabel Ptype where showLabel _ = "ptype"

ptype :: Ptype
ptype = proxy

---------------------------------------------------------------------------
-- Allegiance Field
---------------------------------------------------------------------------

data AllegianceTag
type Allegiance = Proxy AllegianceTag
instance ShowLabel Allegiance where showLabel _ = "allegiance"

allegiance :: Allegiance
allegiance = proxy

---------------------------------------------------------------------------
-- Tag Field
---------------------------------------------------------------------------

data TagTag
type Tag = Proxy TagTag
instance ShowLabel Tag where showLabel _ = "tag"

tag :: Tag
tag = proxy

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
-- Sp Field
---------------------------------------------------------------------------

data SpTag
type Sp = Proxy SpTag
instance ShowLabel Sp where showLabel _ = "sp"

sp :: Sp
sp = proxy

---------------------------------------------------------------------------
-- Start_tick Field
---------------------------------------------------------------------------

data Start_tickTag
type Start_tick = Proxy Start_tickTag
instance ShowLabel Start_tick where showLabel _ = "start_tick"

start_tick :: Start_tick
start_tick = proxy

---------------------------------------------------------------------------
-- Animation_speed Field
---------------------------------------------------------------------------

data Animation_speedTag
type Animation_speed = Proxy Animation_speedTag
instance ShowLabel Animation_speed where
    showLabel _ = "animation_speed"

animation_speed :: Animation_speed
animation_speed = proxy

---------------------------------------------------------------------------
-- Selected Field
---------------------------------------------------------------------------

data SelectedTag
type Selected = Proxy SelectedTag
instance ShowLabel Selected where showLabel _ = "selected"

selected :: Selected
selected = proxy
