{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Squares_valid_categories where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Squares_valid_categories =
     Record
       (HCons (LVPair Category (Expr (Maybe String)))
          (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int))) HNil)))
 
squares_valid_categories :: Table Squares_valid_categories
squares_valid_categories = baseTable "squares_valid_categories"