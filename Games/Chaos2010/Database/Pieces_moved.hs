{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Pieces_moved where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Pieces_moved =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int)) HNil)))
 
pieces_moved :: Table Pieces_moved
pieces_moved = baseTable "pieces_moved"