{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack53  #-}
module Games.Chaos2010.Database.Action_history_mr where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_history_mr =
     Record
       (HCons (LVPair Id (Expr Int))
          (HCons (LVPair History_name (Expr String))
             (HCons (LVPair Allegiance (Expr (Maybe String)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Tx (Expr (Maybe Int)))
                         (HCons (LVPair Ty (Expr (Maybe Int)))
                            (HCons (LVPair Ptype (Expr (Maybe String)))
                               (HCons (LVPair Tag (Expr (Maybe Int)))
                                  (HCons (LVPair Spell_name (Expr (Maybe String)))
                                     (HCons (LVPair Num_wizards (Expr (Maybe Int)))
                                        (HCons (LVPair Turn_number (Expr (Maybe Int)))
                                           (HCons (LVPair Turn_phase (Expr (Maybe String)))
                                              HNil)))))))))))))
 
action_history_mr :: Table Action_history_mr
action_history_mr = baseTable "action_history_mr"