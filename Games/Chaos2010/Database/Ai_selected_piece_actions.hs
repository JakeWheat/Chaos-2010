{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Ai_selected_piece_actions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Ai_selected_piece_actions =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String))) HNil)))
 
ai_selected_piece_actions :: Table Ai_selected_piece_actions
ai_selected_piece_actions = baseTable "ai_selected_piece_actions"