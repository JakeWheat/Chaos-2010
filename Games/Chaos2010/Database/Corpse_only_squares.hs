{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Corpse_only_squares where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Corpse_only_squares =
    Record (HCons (LVPair X (Expr (Maybe Int)))
            (HCons (LVPair Y (Expr (Maybe Int))) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
corpse_only_squares :: Table Corpse_only_squares
corpse_only_squares = baseTable "corpse_only_squares"

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
