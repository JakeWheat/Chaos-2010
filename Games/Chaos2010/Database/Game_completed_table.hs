{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Game_completed_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Game_completed_table =
     Record (HCons (LVPair Game_completed (Expr Bool)) HNil)
 
game_completed_table :: Table Game_completed_table
game_completed_table = baseTable "game_completed_table"