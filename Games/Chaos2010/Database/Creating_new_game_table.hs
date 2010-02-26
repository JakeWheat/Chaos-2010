{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Creating_new_game_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Creating_new_game_table =
     Record (HCons (LVPair Creating_new_game (Expr Bool)) HNil)
 
creating_new_game_table :: Table Creating_new_game_table
creating_new_game_table = baseTable "creating_new_game_table"