{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46  #-}
module Games.Chaos2010.Database.Pieces_with_priorities where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Pieces_with_priorities =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Sp (Expr (Maybe Int))) HNil))))))
 
pieces_with_priorities :: Table Pieces_with_priorities
pieces_with_priorities = baseTable "pieces_with_priorities"