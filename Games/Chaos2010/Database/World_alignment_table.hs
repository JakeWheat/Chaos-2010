{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.World_alignment_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type World_alignment_table =
     Record (HCons (LVPair World_alignment (Expr Int)) HNil)
 
world_alignment_table :: Table World_alignment_table
world_alignment_table = baseTable "world_alignment_table"