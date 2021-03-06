{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack48  #-}
module Games.Chaos2010.Database.Action_history_spell_target where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_spell_target =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair Spell_name (Expr (Maybe String)))
                   (HCons (LVPair X (Expr (Maybe Int)))
                      (HCons (LVPair Y (Expr (Maybe Int)))
                         (HCons (LVPair Tx (Expr (Maybe Int)))
                            (HCons (LVPair Ty (Expr (Maybe Int))) HNil))))))))
 
action_history_spell_target :: Table Action_history_spell_target
action_history_spell_target
  = baseTable "action_history_spell_target"