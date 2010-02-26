{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Current_wizard_spell where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_spell =
     Record (HCons (LVPair Spell_name (Expr (Maybe String))) HNil)
 
current_wizard_spell :: Table Current_wizard_spell
current_wizard_spell = baseTable "current_wizard_spell"