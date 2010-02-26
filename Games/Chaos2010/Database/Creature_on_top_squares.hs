{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Creature_on_top_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Creature_on_top_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
creature_on_top_squares :: Table Creature_on_top_squares
creature_on_top_squares = baseTable "creature_on_top_squares"