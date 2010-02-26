{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Object_piece_types where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Object_piece_types =
     Record (HCons (LVPair Ptype (Expr (Maybe String))) HNil)
 
object_piece_types :: Table Object_piece_types
object_piece_types = baseTable "object_piece_types"