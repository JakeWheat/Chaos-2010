{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Spell_book_show_all_table where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Spell_book_show_all_table =
    Record (HCons (LVPair Spell_book_show_all (Expr Bool)) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
spell_book_show_all_table :: Table Spell_book_show_all_table
spell_book_show_all_table = baseTable "spell_book_show_all_table"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Spell_book_show_all Field
---------------------------------------------------------------------------

data Spell_book_show_allTag
type Spell_book_show_all = Proxy Spell_book_show_allTag
instance ShowLabel Spell_book_show_all where
    showLabel _ = "spell_book_show_all"

spell_book_show_all :: Spell_book_show_all
spell_book_show_all = proxy
