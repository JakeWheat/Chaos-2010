{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.In_next_phase_hack_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type In_next_phase_hack_table =
     Record (HCons (LVPair In_next_phase_hack (Expr Bool)) HNil)
 
in_next_phase_hack_table :: Table In_next_phase_hack_table
in_next_phase_hack_table = baseTable "in_next_phase_hack_table"