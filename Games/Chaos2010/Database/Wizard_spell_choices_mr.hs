{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Wizard_spell_choices_mr where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_spell_choices_mr =
     Record
       (HCons (LVPair Wizard_name (Expr String))
          (HCons (LVPair Spell_name (Expr String))
             (HCons (LVPair Imaginary (Expr (Maybe Bool))) HNil)))
 
wizard_spell_choices_mr :: Table Wizard_spell_choices_mr
wizard_spell_choices_mr = baseTable "wizard_spell_choices_mr"