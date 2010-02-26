{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44  #-}
module Games.Chaos2010.Database.Piece_starting_ticks where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Piece_starting_ticks =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int))
                (HCons (LVPair Start_tick (Expr Int)) HNil))))
 
piece_starting_ticks :: Table Piece_starting_ticks
piece_starting_ticks = baseTable "piece_starting_ticks"