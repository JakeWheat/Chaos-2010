{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Wizard_spell_choices where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_spell_choices =
     Record
       (HCons (LVPair Wizard_name (Expr (Maybe String)))
          (HCons (LVPair Spell_name (Expr (Maybe String))) HNil))
 
wizard_spell_choices :: Table Wizard_spell_choices
wizard_spell_choices = baseTable "wizard_spell_choices"