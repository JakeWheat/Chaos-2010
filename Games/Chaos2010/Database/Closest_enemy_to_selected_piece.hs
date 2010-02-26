{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Closest_enemy_to_selected_piece
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Closest_enemy_to_selected_piece =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
closest_enemy_to_selected_piece ::
                                Table Closest_enemy_to_selected_piece
closest_enemy_to_selected_piece
  = baseTable "closest_enemy_to_selected_piece"