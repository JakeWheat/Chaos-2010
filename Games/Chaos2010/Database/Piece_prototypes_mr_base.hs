{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Piece_prototypes_mr_base where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Piece_prototypes_mr_base =
     Record (HCons (LVPair Ptype (Expr (Maybe String))) HNil)
 
piece_prototypes_mr_base :: Table Piece_prototypes_mr_base
piece_prototypes_mr_base = baseTable "piece_prototypes_mr_base"