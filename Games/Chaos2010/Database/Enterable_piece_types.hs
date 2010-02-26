{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Enterable_piece_types where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Enterable_piece_types =
     Record (HCons (LVPair Ptype (Expr (Maybe String))) HNil)
 
enterable_piece_types :: Table Enterable_piece_types
enterable_piece_types = baseTable "enterable_piece_types"