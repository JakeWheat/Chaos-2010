{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Ai_filtered_target_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Ai_filtered_target_spells =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String))) HNil)))
 
ai_filtered_target_spells :: Table Ai_filtered_target_spells
ai_filtered_target_spells = baseTable "ai_filtered_target_spells"