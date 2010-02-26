{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spreadable_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spreadable_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
spreadable_squares :: Table Spreadable_squares
spreadable_squares = baseTable "spreadable_squares"