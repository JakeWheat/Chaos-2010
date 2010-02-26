{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Spell_book_show_all_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_book_show_all_table =
     Record (HCons (LVPair Spell_book_show_all (Expr Bool)) HNil)
 
spell_book_show_all_table :: Table Spell_book_show_all_table
spell_book_show_all_table = baseTable "spell_book_show_all_table"