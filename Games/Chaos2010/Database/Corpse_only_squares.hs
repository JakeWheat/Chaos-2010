{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Corpse_only_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Corpse_only_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
corpse_only_squares :: Table Corpse_only_squares
corpse_only_squares = baseTable "corpse_only_squares"