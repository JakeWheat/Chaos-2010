{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Imaginary_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Imaginary_pieces =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int)) HNil)))
 
imaginary_pieces :: Table Imaginary_pieces
imaginary_pieces = baseTable "imaginary_pieces"