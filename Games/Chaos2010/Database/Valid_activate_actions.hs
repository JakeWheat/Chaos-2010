{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Valid_activate_actions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Valid_activate_actions =
     Record (HCons (LVPair Action (Expr (Maybe String))) HNil)
 
valid_activate_actions :: Table Valid_activate_actions
valid_activate_actions = baseTable "valid_activate_actions"