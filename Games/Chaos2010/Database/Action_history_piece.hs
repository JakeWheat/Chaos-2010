{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Action_history_piece where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_piece =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair Ptype (Expr (Maybe String)))
                   (HCons (LVPair Tag (Expr (Maybe Int))) HNil)))))
 
action_history_piece :: Table Action_history_piece
action_history_piece = baseTable "action_history_piece"