{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spell_cast_chance where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_cast_chance =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Chance (Expr (Maybe Int))) HNil))
 
spell_cast_chance :: Table Spell_cast_chance
spell_cast_chance = baseTable "spell_cast_chance"