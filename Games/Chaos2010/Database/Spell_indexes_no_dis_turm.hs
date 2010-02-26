{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spell_indexes_no_dis_turm where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_indexes_no_dis_turm =
     Record
       (HCons (LVPair Row_number (Expr Int))
          (HCons (LVPair Spell_name (Expr String)) HNil))
 
spell_indexes_no_dis_turm :: Table Spell_indexes_no_dis_turm
spell_indexes_no_dis_turm = baseTable "spell_indexes_no_dis_turm"