{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Empty_and_not_adjacent_to_tree_squares
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Empty_and_not_adjacent_to_tree_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
empty_and_not_adjacent_to_tree_squares ::
                                       Table Empty_and_not_adjacent_to_tree_squares
empty_and_not_adjacent_to_tree_squares
  = baseTable "empty_and_not_adjacent_to_tree_squares"