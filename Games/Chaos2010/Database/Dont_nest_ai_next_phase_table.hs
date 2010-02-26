{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Dont_nest_ai_next_phase_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Dont_nest_ai_next_phase_table =
     Record (HCons (LVPair Dont_nest_ai_next_phase (Expr Bool)) HNil)
 
dont_nest_ai_next_phase_table ::
                              Table Dont_nest_ai_next_phase_table
dont_nest_ai_next_phase_table
  = baseTable "dont_nest_ai_next_phase_table"