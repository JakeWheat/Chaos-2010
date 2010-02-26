{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Turn_phase_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Turn_phase_table =
     Record (HCons (LVPair Turn_phase (Expr String)) HNil)
 
turn_phase_table :: Table Turn_phase_table
turn_phase_table = baseTable "turn_phase_table"