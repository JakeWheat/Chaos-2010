{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Turn_number_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Turn_number_table =
     Record (HCons (LVPair Turn_number (Expr Int)) HNil)
 
turn_number_table :: Table Turn_number_table
turn_number_table = baseTable "turn_number_table"