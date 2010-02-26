{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Action_history_num_wiz where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_num_wiz =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Num_wizards (Expr (Maybe Int))) HNil)))
 
action_history_num_wiz :: Table Action_history_num_wiz
action_history_num_wiz = baseTable "action_history_num_wiz"