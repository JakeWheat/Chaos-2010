{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Adjacent_to_tree_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Adjacent_to_tree_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
adjacent_to_tree_squares :: Table Adjacent_to_tree_squares
adjacent_to_tree_squares = baseTable "adjacent_to_tree_squares"