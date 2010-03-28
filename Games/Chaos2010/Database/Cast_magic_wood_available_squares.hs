{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42    #-}
module Games.Chaos2010.Database.Cast_magic_wood_available_squares
       where
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Cast_magic_wood_available_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
cast_magic_wood_available_squares ::
                                  Table Cast_magic_wood_available_squares
cast_magic_wood_available_squares
  = baseTable "cast_magic_wood_available_squares"