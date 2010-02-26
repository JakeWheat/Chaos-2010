{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack50  #-}
module Games.Chaos2010.Database.Range_attacking_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Range_attacking_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Attack_strength (Expr (Maybe Int)))
             (HCons (LVPair Physical_defense (Expr (Maybe Int)))
                (HCons (LVPair Magic_defense (Expr (Maybe Int)))
                   (HCons (LVPair Flying (Expr (Maybe Bool)))
                      (HCons (LVPair Speed (Expr (Maybe Int)))
                         (HCons (LVPair Agility (Expr (Maybe Int)))
                            (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                               (HCons (LVPair Range (Expr (Maybe Int)))
                                  (HCons (LVPair Ranged_attack_strength (Expr (Maybe Int)))
                                     HNil))))))))))
 
range_attacking_prototypes :: Table Range_attacking_prototypes
range_attacking_prototypes = baseTable "range_attacking_prototypes"