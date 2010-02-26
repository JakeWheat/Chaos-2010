{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Cursor_position where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Cursor_position =
     Record
       (HCons (LVPair X (Expr Int)) (HCons (LVPair Y (Expr Int)) HNil))
 
cursor_position :: Table Cursor_position
cursor_position = baseTable "cursor_position"