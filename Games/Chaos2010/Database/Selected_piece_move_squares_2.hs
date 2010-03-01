{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Selected_piece_move_squares_2 where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Selected_piece_move_squares_2 =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String))) HNil)))
 
selected_piece_move_squares_2 ::
                              Table Selected_piece_move_squares_2
selected_piece_move_squares_2
  = baseTable "selected_piece_move_squares_2"