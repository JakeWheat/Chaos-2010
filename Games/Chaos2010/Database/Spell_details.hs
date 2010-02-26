{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack59  #-}
module Games.Chaos2010.Database.Spell_details where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_details =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Base_chance (Expr (Maybe Int)))
             (HCons (LVPair Alignment (Expr (Maybe Int)))
                (HCons (LVPair Spell_category (Expr (Maybe String)))
                   (HCons (LVPair Description (Expr (Maybe String)))
                      (HCons (LVPair Range (Expr (Maybe Int)))
                         (HCons (LVPair Numb (Expr (Maybe Int)))
                            (HCons (LVPair Valid_square_category (Expr (Maybe String)))
                               (HCons (LVPair Ptype (Expr (Maybe String)))
                                  (HCons (LVPair Sprite (Expr (Maybe String)))
                                     (HCons (LVPair Count (Expr (Maybe Int)))
                                        (HCons (LVPair Chance (Expr (Maybe Int)))
                                           (HCons (LVPair Alignment_string (Expr (Maybe String)))
                                              (HCons (LVPair Key (Expr (Maybe String)))
                                                 (HCons (LVPair Section_order (Expr (Maybe Int)))
                                                    (HCons
                                                       (LVPair Alignment_order (Expr (Maybe Int)))
                                                       (HCons
                                                          (LVPair Count_icons (Expr (Maybe String)))
                                                          (HCons
                                                             (LVPair Align_icons
                                                                (Expr (Maybe String)))
                                                             (HCons
                                                                (LVPair Colour
                                                                   (Expr (Maybe String)))
                                                                HNil)))))))))))))))))))
 
spell_details :: Table Spell_details
spell_details = baseTable "spell_details"