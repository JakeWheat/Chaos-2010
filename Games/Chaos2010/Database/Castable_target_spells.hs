{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Castable_target_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Castable_target_spells =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int))) HNil)))
 
castable_target_spells :: Table Castable_target_spells
castable_target_spells = baseTable "castable_target_spells"