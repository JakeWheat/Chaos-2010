{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Action_history_turn_num where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_turn_num =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Turn_number (Expr (Maybe Int))) HNil)))
 
action_history_turn_num :: Table Action_history_turn_num
action_history_turn_num = baseTable "action_history_turn_num"