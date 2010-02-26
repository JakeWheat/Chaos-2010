{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Client_valid_activate_actions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Client_valid_activate_actions =
     Record (HCons (LVPair Action (Expr (Maybe String))) HNil)
 
client_valid_activate_actions ::
                              Table Client_valid_activate_actions
client_valid_activate_actions
  = baseTable "client_valid_activate_actions"