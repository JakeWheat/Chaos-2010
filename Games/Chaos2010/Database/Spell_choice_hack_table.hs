{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41     #-}
module Games.Chaos2010.Database.Spell_choice_hack_table where
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_choice_hack_table =
     Record (HCons (LVPair Spell_choice_hack (Expr Bool)) HNil)
 
spell_choice_hack_table :: Table Spell_choice_hack_table
spell_choice_hack_table = baseTable "spell_choice_hack_table"