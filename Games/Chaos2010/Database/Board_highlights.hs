{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Board_highlights where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Board_highlights =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Sprite (Expr (Maybe String))) HNil)))
 
board_highlights :: Table Board_highlights
board_highlights = baseTable "board_highlights"