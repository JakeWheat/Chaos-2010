{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Monster_on_top_squares where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Monster_on_top_squares =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int))) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
monster_on_top_squares :: Table Monster_on_top_squares
monster_on_top_squares = baseTable "monster_on_top_squares"

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
