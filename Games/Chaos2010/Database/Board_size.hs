{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Board_size where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Board_size =
    Record (HCons (LVPair Width (Expr Int))
            (HCons (LVPair Height (Expr Int)) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
board_size :: Table Board_size
board_size = baseTable "board_size"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Width Field
---------------------------------------------------------------------------

data WidthTag
type Width = Proxy WidthTag
instance ShowLabel Width where showLabel _ = "width"

width :: Width
width = proxy

---------------------------------------------------------------------------
-- Height Field
---------------------------------------------------------------------------

data HeightTag
type Height = Proxy HeightTag
instance ShowLabel Height where showLabel _ = "height"

height :: Height
height = proxy