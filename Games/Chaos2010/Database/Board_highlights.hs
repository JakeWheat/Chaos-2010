{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Board_highlights where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Board_highlights =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Sprite (Expr (Maybe String))) HNil)))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
board_highlights :: Table Board_highlights
board_highlights = baseTable "board_highlights"

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
-- Sprite Field
---------------------------------------------------------------------------

data SpriteTag
type Sprite = Proxy SpriteTag
instance ShowLabel Sprite where showLabel _ = "sprite"

sprite :: Sprite
sprite = proxy