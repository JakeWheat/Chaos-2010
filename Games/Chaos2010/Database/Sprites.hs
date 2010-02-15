{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Sprites where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Sprites =
    Record (HCons (LVPair Sprite (Expr String))
            (HCons (LVPair Animation_speed (Expr Int)) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
sprites :: Table Sprites
sprites = baseTable "sprites"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Sprite Field
---------------------------------------------------------------------------

data SpriteTag
type Sprite = Proxy SpriteTag
instance ShowLabel Sprite where showLabel _ = "sprite"

sprite :: Sprite
sprite = proxy

---------------------------------------------------------------------------
-- Animation_speed Field
---------------------------------------------------------------------------

data Animation_speedTag
type Animation_speed = Proxy Animation_speedTag
instance ShowLabel Animation_speed where
    showLabel _ = "animation_speed"

animation_speed :: Animation_speed
animation_speed = proxy
