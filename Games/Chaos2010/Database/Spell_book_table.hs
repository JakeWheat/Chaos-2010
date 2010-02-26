{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack54  #-}
module Games.Chaos2010.Database.Spell_book_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_book_table =
     Record
       (HCons (LVPair Spell_category (Expr (Maybe String)))
          (HCons (LVPair Spell_name (Expr (Maybe String)))
             (HCons (LVPair Count (Expr (Maybe Int)))
                (HCons (LVPair Chance (Expr (Maybe Int)))
                   (HCons (LVPair Alignment (Expr (Maybe Int)))
                      (HCons (LVPair Alignment_string (Expr (Maybe String)))
                         (HCons (LVPair Key (Expr (Maybe String)))
                            (HCons (LVPair Sprite (Expr (Maybe String)))
                               (HCons (LVPair Section_order (Expr (Maybe Int)))
                                  (HCons (LVPair Alignment_order (Expr (Maybe Int)))
                                     (HCons (LVPair Base_chance (Expr (Maybe Int)))
                                        (HCons (LVPair Count_icons (Expr (Maybe String)))
                                           (HCons (LVPair Align_icons (Expr (Maybe String)))
                                              (HCons (LVPair Colour (Expr (Maybe String)))
                                                 HNil))))))))))))))
 
spell_book_table :: Table Spell_book_table
spell_book_table = baseTable "spell_book_table"