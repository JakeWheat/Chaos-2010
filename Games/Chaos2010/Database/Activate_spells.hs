{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Activate_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Activate_spells =
     Record (HCons (LVPair Spell_name (Expr (Maybe String))) HNil)
 
activate_spells :: Table Activate_spells
activate_spells = baseTable "activate_spells"