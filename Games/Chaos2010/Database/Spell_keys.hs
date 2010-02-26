{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spell_keys where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_keys =
     Record
       (HCons (LVPair Spell_name (Expr String))
          (HCons (LVPair Key (Expr String)) HNil))
 
spell_keys :: Table Spell_keys
spell_keys = baseTable "spell_keys"