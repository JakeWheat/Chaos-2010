{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack48  #-}
module Games.Chaos2010.Database.Ranged_weapon_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Ranged_weapon_pieces =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                         (HCons (LVPair Range (Expr (Maybe Int)))
                            (HCons (LVPair Ranged_attack_strength (Expr (Maybe Int)))
                               HNil))))))))
 
ranged_weapon_pieces :: Table Ranged_weapon_pieces
ranged_weapon_pieces = baseTable "ranged_weapon_pieces"