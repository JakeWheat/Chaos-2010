{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Valid_target_actions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Valid_target_actions =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String))) HNil)))
 
valid_target_actions :: Table Valid_target_actions
valid_target_actions = baseTable "valid_target_actions"