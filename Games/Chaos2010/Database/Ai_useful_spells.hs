{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Ai_useful_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Ai_useful_spells =
     Record (HCons (LVPair Spell_name (Expr (Maybe String))) HNil)
 
ai_useful_spells :: Table Ai_useful_spells
ai_useful_spells = baseTable "ai_useful_spells"