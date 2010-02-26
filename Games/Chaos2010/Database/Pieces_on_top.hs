{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46  #-}
module Games.Chaos2010.Database.Pieces_on_top where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Pieces_on_top =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Ptype (Expr (Maybe String)))
                (HCons (LVPair Allegiance (Expr (Maybe String)))
                   (HCons (LVPair Tag (Expr (Maybe Int)))
                      (HCons (LVPair Sp (Expr (Maybe Int))) HNil))))))
 
pieces_on_top :: Table Pieces_on_top
pieces_on_top = baseTable "pieces_on_top"