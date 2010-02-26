{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Remaining_walk_hack_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Remaining_walk_hack_table =
     Record (HCons (LVPair Remaining_walk_hack (Expr Bool)) HNil)
 
remaining_walk_hack_table :: Table Remaining_walk_hack_table
remaining_walk_hack_table = baseTable "remaining_walk_hack_table"