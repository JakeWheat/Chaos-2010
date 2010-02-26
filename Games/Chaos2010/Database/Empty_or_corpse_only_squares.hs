{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Empty_or_corpse_only_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Empty_or_corpse_only_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
empty_or_corpse_only_squares :: Table Empty_or_corpse_only_squares
empty_or_corpse_only_squares
  = baseTable "empty_or_corpse_only_squares"