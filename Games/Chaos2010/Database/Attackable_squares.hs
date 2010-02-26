{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Attackable_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Attackable_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
attackable_squares :: Table Attackable_squares
attackable_squares = baseTable "attackable_squares"