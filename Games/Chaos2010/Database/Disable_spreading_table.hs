{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Disable_spreading_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Disable_spreading_table =
     Record (HCons (LVPair Disable_spreading (Expr Bool)) HNil)
 
disable_spreading_table :: Table Disable_spreading_table
disable_spreading_table = baseTable "disable_spreading_table"