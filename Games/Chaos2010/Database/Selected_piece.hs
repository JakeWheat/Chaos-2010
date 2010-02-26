{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Selected_piece where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Selected_piece =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int))
                (HCons (LVPair Move_phase (Expr String))
                   (HCons (LVPair Engaged (Expr Bool)) HNil)))))
 
selected_piece :: Table Selected_piece
selected_piece = baseTable "selected_piece"