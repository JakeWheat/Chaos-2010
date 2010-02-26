{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Current_wizard_spell_range_squares
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_spell_range_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
current_wizard_spell_range_squares ::
                                   Table Current_wizard_spell_range_squares
current_wizard_spell_range_squares
  = baseTable "current_wizard_spell_range_squares"