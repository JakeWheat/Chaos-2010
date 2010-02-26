{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Cast_success_checked_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Cast_success_checked_table =
     Record (HCons (LVPair Cast_success_checked (Expr Bool)) HNil)
 
cast_success_checked_table :: Table Cast_success_checked_table
cast_success_checked_table = baseTable "cast_success_checked_table"