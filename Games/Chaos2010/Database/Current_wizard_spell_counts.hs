{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Current_wizard_spell_counts where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_spell_counts =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Count (Expr (Maybe Int))) HNil))
 
current_wizard_spell_counts :: Table Current_wizard_spell_counts
current_wizard_spell_counts
  = baseTable "current_wizard_spell_counts"