{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Board_ranges where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Board_ranges =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Range (Expr (Maybe Int)))
                (HCons (LVPair Tx (Expr (Maybe Int)))
                   (HCons (LVPair Ty (Expr (Maybe Int))) HNil)))))
 
board_ranges :: Table Board_ranges
board_ranges = baseTable "board_ranges"