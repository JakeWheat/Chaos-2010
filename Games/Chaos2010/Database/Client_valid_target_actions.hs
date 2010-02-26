{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Client_valid_target_actions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Client_valid_target_actions =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String))) HNil)))
 
client_valid_target_actions :: Table Client_valid_target_actions
client_valid_target_actions
  = baseTable "client_valid_target_actions"