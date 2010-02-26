{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Squares_within_selected_piece_flight_range
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Squares_within_selected_piece_flight_range =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
squares_within_selected_piece_flight_range ::
                                           Table Squares_within_selected_piece_flight_range
squares_within_selected_piece_flight_range
  = baseTable "squares_within_selected_piece_flight_range"