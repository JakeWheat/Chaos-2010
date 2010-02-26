{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44  #-}
module Games.Chaos2010.Database.Action_history_spell where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_spell =
     Record
       (HCons (LVPair Id (Expr (Maybe Int)))
          (HCons (LVPair History_name (Expr (Maybe String)))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair Spell_name (Expr (Maybe String))) HNil))))
 
action_history_spell :: Table Action_history_spell
action_history_spell = baseTable "action_history_spell"