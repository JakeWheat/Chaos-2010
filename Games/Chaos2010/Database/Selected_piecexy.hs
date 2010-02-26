{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack47  #-}
module Games.Chaos2010.Database.Selected_piecexy where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Selected_piecexy =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair Move_phase (Expr (Maybe String)))
                   (HCons (LVPair Engaged (Expr (Maybe Bool)))
                      (HCons (LVPair X (Expr (Maybe Int)))
                         (HCons (LVPair Y (Expr (Maybe Int))) HNil)))))))
 
selected_piecexy :: Table Selected_piecexy
selected_piecexy = baseTable "selected_piecexy"