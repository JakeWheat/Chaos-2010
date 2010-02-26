{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Spell_books where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_books =
     Record
       (HCons (LVPair Id (Expr Int))
          (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Spell_name (Expr String)) HNil)))
 
spell_books :: Table Spell_books
spell_books = baseTable "spell_books"