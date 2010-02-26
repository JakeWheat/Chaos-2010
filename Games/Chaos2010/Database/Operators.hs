{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Operators where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Operators =
     Record (HCons (LVPair Operator_name (Expr (Maybe String))) HNil)
 
operators :: Table Operators
operators = baseTable "operators"