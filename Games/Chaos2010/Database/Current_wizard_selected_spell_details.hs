{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack50  #-}
module Games.Chaos2010.Database.Current_wizard_selected_spell_details
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_selected_spell_details =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Spell_category (Expr (Maybe String)))
             (HCons (LVPair Sprite (Expr (Maybe String)))
                (HCons (LVPair Base_chance (Expr (Maybe Int)))
                   (HCons (LVPair Description (Expr (Maybe String)))
                      (HCons (LVPair Numb (Expr (Maybe Int)))
                         (HCons (LVPair Range (Expr (Maybe Int)))
                            (HCons (LVPair Count (Expr (Maybe Int)))
                               (HCons (LVPair Chance (Expr (Maybe Int)))
                                  (HCons (LVPair Alignment_string (Expr (Maybe String)))
                                     HNil))))))))))
 
current_wizard_selected_spell_details ::
                                      Table Current_wizard_selected_spell_details
current_wizard_selected_spell_details
  = baseTable "current_wizard_selected_spell_details"