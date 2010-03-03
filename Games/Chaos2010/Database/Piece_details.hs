{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack72  #-}
module Games.Chaos2010.Database.Piece_details where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Piece_details =
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
                                                          (HCons
                                                             (LVPair Wtype (Expr (Maybe String)))
                                                             (HCons
                                                                (LVPair Wizard_name
                                                                   (Expr (Maybe String)))
                                                                (HCons
                                                                   (LVPair Shadow_form
                                                                      (Expr (Maybe Bool)))
                                                                   (HCons
                                                                      (LVPair Magic_sword
                                                                         (Expr (Maybe Bool)))
                                                                      (HCons
                                                                         (LVPair Magic_knife
                                                                            (Expr (Maybe Bool)))
                                                                         (HCons
                                                                            (LVPair Magic_shield
                                                                               (Expr (Maybe Bool)))
                                                                            (HCons
                                                                               (LVPair Magic_wings
                                                                                  (Expr
                                                                                     (Maybe Bool)))
                                                                               (HCons
                                                                                  (LVPair
                                                                                     Magic_armour
                                                                                     (Expr
                                                                                        (Maybe
                                                                                           Bool)))
                                                                                  (HCons
                                                                                     (LVPair
                                                                                        Magic_bow
                                                                                        (Expr
                                                                                           (Maybe
                                                                                              Bool)))
                                                                                     (HCons
                                                                                        (LVPair
                                                                                           Computer_controlled
                                                                                           (Expr
                                                                                              (Maybe
                                                                                                 Bool)))
                                                                                        (HCons
                                                                                           (LVPair
                                                                                              Original_place
                                                                                              (Expr
                                                                                                 (Maybe
                                                                                                    Int)))
                                                                                           (HCons
                                                                                              (LVPair
                                                                                                 Expired
                                                                                                 (Expr
                                                                                                    (Maybe
                                                                                                       Bool)))
                                                                                              (HCons
                                                                                                 (LVPair
                                                                                                    Sp
                                                                                                    (Expr
                                                                                                       (Maybe
                                                                                                          Int)))
                                                                                                 (HCons
                                                                                                    (LVPair
                                                                                                       Sprite
                                                                                                       (Expr
                                                                                                          (Maybe
                                                                                                             String)))
                                                                                                    (HCons
                                                                                                       (LVPair
                                                                                                          Colour
                                                                                                          (Expr
                                                                                                             (Maybe
                                                                                                                String)))
                                                                                                       HNil))))))))))))))))))))))))))))))))
 
piece_details :: Table Piece_details
piece_details = baseTable "piece_details"