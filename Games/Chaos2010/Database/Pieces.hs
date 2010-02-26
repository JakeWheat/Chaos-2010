{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Pieces =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int))
                (HCons (LVPair X (Expr Int)) (HCons (LVPair Y (Expr Int)) HNil)))))
 
pieces :: Table Pieces
pieces = baseTable "pieces"