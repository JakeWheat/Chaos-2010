{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Select_best_move where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Select_best_move =
     Record
       (HCons (LVPair Action (Expr (Maybe String)))
          (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int))) HNil)))
 
select_best_move :: Table Select_best_move
select_best_move = baseTable "select_best_move"