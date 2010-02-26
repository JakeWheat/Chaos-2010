{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Wizard_spell_choices_imaginary
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_spell_choices_imaginary =
     Record
       (HCons (LVPair Wizard_name (Expr (Maybe String)))
          (HCons (LVPair Spell_name (Expr (Maybe String)))
             (HCons (LVPair Imaginary (Expr (Maybe Bool))) HNil)))
 
wizard_spell_choices_imaginary ::
                               Table Wizard_spell_choices_imaginary
wizard_spell_choices_imaginary
  = baseTable "wizard_spell_choices_imaginary"