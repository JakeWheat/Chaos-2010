{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Base_relvars where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Base_relvars =
     Record (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil)
 
base_relvars :: Table Base_relvars
base_relvars = baseTable "base_relvars"