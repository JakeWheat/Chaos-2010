{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack52  #-}
module Games.Chaos2010.Database.Piece_prototypes_mr where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Piece_prototypes_mr =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Attack_strength (Expr (Maybe Int)))
             (HCons (LVPair Physical_defense (Expr (Maybe Int)))
                (HCons (LVPair Magic_defense (Expr (Maybe Int)))
                   (HCons (LVPair Flying (Expr (Maybe Bool)))
                      (HCons (LVPair Speed (Expr (Maybe Int)))
                         (HCons (LVPair Agility (Expr (Maybe Int)))
                            (HCons (LVPair Undead (Expr (Maybe Bool)))
                               (HCons (LVPair Ridable (Expr (Maybe Bool)))
                                  (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                                     (HCons (LVPair Range (Expr (Maybe Int)))
                                        (HCons (LVPair Ranged_attack_strength (Expr (Maybe Int)))
                                           HNil))))))))))))
 
piece_prototypes_mr :: Table Piece_prototypes_mr
piece_prototypes_mr = baseTable "piece_prototypes_mr"