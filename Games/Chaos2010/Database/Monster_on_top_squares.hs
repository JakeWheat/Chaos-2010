{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Monster_on_top_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Monster_on_top_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
monster_on_top_squares :: Table Monster_on_top_squares
monster_on_top_squares = baseTable "monster_on_top_squares"