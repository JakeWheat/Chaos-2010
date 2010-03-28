{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack60  #-}
module Games.Chaos2010.Database.Triggers where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Triggers =
     Record
       (HCons (LVPair Relvar_name (Expr (Maybe String)))
          (HCons (LVPair Trigger_catalog (Expr (Maybe String)))
             (HCons (LVPair Trigger_name (Expr (Maybe String)))
                (HCons (LVPair Trigger_schema (Expr (Maybe String)))
                   (HCons (LVPair Operator_name (Expr (Maybe String)))
                      (HCons (LVPair Trigger_name (Expr (Maybe String)))
                         (HCons (LVPair Event_manipulation (Expr (Maybe String)))
                            (HCons (LVPair Event_object_catalog (Expr (Maybe String)))
                               (HCons (LVPair Event_object_schema (Expr (Maybe String)))
                                  (HCons (LVPair Event_object_table (Expr (Maybe String)))
                                     (HCons (LVPair Action_order (Expr (Maybe String)))
                                        (HCons (LVPair Action_condition (Expr (Maybe String)))
                                           (HCons (LVPair Action_statement (Expr (Maybe String)))
                                              (HCons
                                                 (LVPair Action_orientation (Expr (Maybe String)))
                                                 (HCons
                                                    (LVPair Condition_timing (Expr (Maybe String)))
                                                    (HCons
                                                       (LVPair Condition_reference_old_table
                                                          (Expr (Maybe String)))
                                                       (HCons
                                                          (LVPair Condition_reference_new_table
                                                             (Expr (Maybe String)))
                                                          (HCons
                                                             (LVPair Condition_reference_old_row
                                                                (Expr (Maybe String)))
                                                             (HCons
                                                                (LVPair Condition_reference_new_row
                                                                   (Expr (Maybe String)))
                                                                (HCons
                                                                   (LVPair Created
                                                                      (Expr (Maybe String)))
                                                                   HNil))))))))))))))))))))
 
triggers :: Table Triggers
triggers = baseTable "triggers"