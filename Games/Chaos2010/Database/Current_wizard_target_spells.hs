{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Current_wizard_target_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_target_spells =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Range (Expr (Maybe Int))) HNil))
 
current_wizard_target_spells :: Table Current_wizard_target_spells
current_wizard_target_spells
  = baseTable "current_wizard_target_spells"