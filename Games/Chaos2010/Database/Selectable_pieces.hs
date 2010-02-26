{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack47  #-}
module Games.Chaos2010.Database.Selectable_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Selectable_pieces =
     Record
       (HCons (LVPair Rn (Expr (Maybe Int)))
          (HCons (LVPair Ptype (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair Tag (Expr (Maybe Int)))
                   (HCons (LVPair X (Expr (Maybe Int)))
                      (HCons (LVPair Y (Expr (Maybe Int)))
                         (HCons (LVPair Sp (Expr (Maybe Int))) HNil)))))))
 
selectable_pieces :: Table Selectable_pieces
selectable_pieces = baseTable "selectable_pieces"