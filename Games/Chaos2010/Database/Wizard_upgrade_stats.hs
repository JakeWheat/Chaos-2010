{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack57  #-}
module Games.Chaos2010.Database.Wizard_upgrade_stats where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_upgrade_stats =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Imaginary (Expr (Maybe Bool)))
                         (HCons (LVPair Flying (Expr (Maybe Bool)))
                            (HCons (LVPair Speed (Expr (Maybe Int)))
                               (HCons (LVPair Agility (Expr (Maybe Int)))
                                  (HCons (LVPair Undead (Expr (Maybe Bool)))
                                     (HCons (LVPair Ridable (Expr (Maybe Bool)))
                                        (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                                           (HCons (LVPair Range (Expr (Maybe Int)))
                                              (HCons
                                                 (LVPair Ranged_attack_strength (Expr (Maybe Int)))
                                                 (HCons (LVPair Attack_strength (Expr (Maybe Int)))
                                                    (HCons
                                                       (LVPair Physical_defense (Expr (Maybe Int)))
                                                       (HCons
                                                          (LVPair Magic_defense (Expr (Maybe Int)))
                                                          HNil)))))))))))))))))
 
wizard_upgrade_stats :: Table Wizard_upgrade_stats
wizard_upgrade_stats = baseTable "wizard_upgrade_stats"