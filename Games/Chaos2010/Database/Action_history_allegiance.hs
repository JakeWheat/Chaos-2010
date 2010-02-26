{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Action_history_allegiance where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_allegiance =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String))) HNil)))
 
action_history_allegiance :: Table Action_history_allegiance
action_history_allegiance = baseTable "action_history_allegiance"