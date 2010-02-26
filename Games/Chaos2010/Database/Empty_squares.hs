{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Empty_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Empty_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
empty_squares :: Table Empty_squares
empty_squares = baseTable "empty_squares"