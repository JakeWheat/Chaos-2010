{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Spell_parts_to_cast_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_parts_to_cast_table =
     Record (HCons (LVPair Spell_parts_to_cast (Expr Int)) HNil)
 
spell_parts_to_cast_table :: Table Spell_parts_to_cast_table
spell_parts_to_cast_table = baseTable "spell_parts_to_cast_table"