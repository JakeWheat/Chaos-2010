{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Test_spell_books where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Test_spell_books =
     Record
       (HCons (LVPair Wizard_name (Expr String))
          (HCons (LVPair Spell_name (Expr String)) HNil))
 
test_spell_books :: Table Test_spell_books
test_spell_books = baseTable "test_spell_books"