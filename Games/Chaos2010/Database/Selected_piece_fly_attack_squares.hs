{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Selected_piece_fly_attack_squares
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Selected_piece_fly_attack_squares =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
selected_piece_fly_attack_squares ::
                                  Table Selected_piece_fly_attack_squares
selected_piece_fly_attack_squares
  = baseTable "selected_piece_fly_attack_squares"