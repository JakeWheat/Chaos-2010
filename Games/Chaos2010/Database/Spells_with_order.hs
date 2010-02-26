{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack47  #-}
module Games.Chaos2010.Database.Spells_with_order where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spells_with_order =
     Record
       (HCons (LVPair Spell_category (Expr (Maybe String)))
          (HCons (LVPair Spell_name (Expr (Maybe String)))
             (HCons (LVPair Base_chance (Expr (Maybe Int)))
                (HCons (LVPair Alignment (Expr (Maybe Int)))
                   (HCons (LVPair Description (Expr (Maybe String)))
                      (HCons (LVPair Section_order (Expr (Maybe Int)))
                         (HCons (LVPair Alignment_order (Expr (Maybe Int))) HNil)))))))
 
spells_with_order :: Table Spells_with_order
spells_with_order = baseTable "spells_with_order"