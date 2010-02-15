{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Selected_piece_walk_squares where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Selected_piece_walk_squares =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int))) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
selected_piece_walk_squares :: Table Selected_piece_walk_squares
selected_piece_walk_squares = baseTable "selected_piece_walk_squares"

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
