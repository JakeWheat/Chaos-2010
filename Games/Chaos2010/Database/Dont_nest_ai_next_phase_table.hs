{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Dont_nest_ai_next_phase_table where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Dont_nest_ai_next_phase_table =
    Record (HCons (LVPair Dont_nest_ai_next_phase (Expr Bool)) HNil)

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
dont_nest_ai_next_phase_table :: Table
    Dont_nest_ai_next_phase_table
dont_nest_ai_next_phase_table = baseTable "dont_nest_ai_next_phase_table"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Dont_nest_ai_next_phase Field
---------------------------------------------------------------------------

data Dont_nest_ai_next_phaseTag
type Dont_nest_ai_next_phase = Proxy Dont_nest_ai_next_phaseTag
instance ShowLabel Dont_nest_ai_next_phase where
    showLabel _ = "dont_nest_ai_next_phase"

dont_nest_ai_next_phase :: Dont_nest_ai_next_phase
dont_nest_ai_next_phase = proxy