{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Remaining_walk_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Remaining_walk_table =
     Record (HCons (LVPair Remaining_walk (Expr Int)) HNil)
 
remaining_walk_table :: Table Remaining_walk_table
remaining_walk_table = baseTable "remaining_walk_table"