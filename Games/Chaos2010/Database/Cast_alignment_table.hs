{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Cast_alignment_table where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Cast_alignment_table =
    Record (HCons (LVPair Cast_alignment (Expr Int)) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
cast_alignment_table :: Table Cast_alignment_table
cast_alignment_table = baseTable "cast_alignment_table"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Cast_alignment Field
---------------------------------------------------------------------------

data Cast_alignmentTag
type Cast_alignment = Proxy Cast_alignmentTag
instance ShowLabel Cast_alignment where
    showLabel _ = "cast_alignment"

cast_alignment :: Cast_alignment
cast_alignment = proxy
