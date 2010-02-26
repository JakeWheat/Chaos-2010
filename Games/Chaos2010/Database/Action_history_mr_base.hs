{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Action_history_mr_base where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_mr_base =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String))) HNil))
 
action_history_mr_base :: Table Action_history_mr_base
action_history_mr_base = baseTable "action_history_mr_base"