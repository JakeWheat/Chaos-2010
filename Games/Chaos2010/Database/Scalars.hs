{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Scalars where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Scalars =
     Record (HCons (LVPair Scalar_name (Expr (Maybe String))) HNil)
 
scalars :: Table Scalars
scalars = baseTable "scalars"