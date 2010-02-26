{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44  #-}
module Games.Chaos2010.Database.Colours where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Colours =
     Record
       (HCons (LVPair Name (Expr String))
          (HCons (LVPair Red (Expr Int))
             (HCons (LVPair Green (Expr Int))
                (HCons (LVPair Blue (Expr Int)) HNil))))
 
colours :: Table Colours
colours = baseTable "colours"