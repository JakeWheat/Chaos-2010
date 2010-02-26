{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46  #-}
module Games.Chaos2010.Database.Action_history_turn_phase where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_turn_phase =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Turn_phase (Expr (Maybe String))) HNil))))))
 
action_history_turn_phase :: Table Action_history_turn_phase
action_history_turn_phase = baseTable "action_history_turn_phase"