{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Cast_alignment_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Cast_alignment_table =
     Record (HCons (LVPair Cast_alignment (Expr Int)) HNil)
 
cast_alignment_table :: Table Cast_alignment_table
cast_alignment_table = baseTable "cast_alignment_table"