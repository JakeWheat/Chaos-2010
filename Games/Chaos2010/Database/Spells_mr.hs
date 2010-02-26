{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack49  #-}
module Games.Chaos2010.Database.Spells_mr where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spells_mr =
     Record
       (HCons (LVPair Spell_name (Expr String))
          (HCons (LVPair Base_chance (Expr Int))
             (HCons (LVPair Alignment (Expr Int))
                (HCons (LVPair Spell_category (Expr String))
                   (HCons (LVPair Description (Expr String))
                      (HCons (LVPair Range (Expr (Maybe Int)))
                         (HCons (LVPair Numb (Expr (Maybe Int)))
                            (HCons (LVPair Valid_square_category (Expr (Maybe String)))
                               (HCons (LVPair Ptype (Expr (Maybe String))) HNil)))))))))
 
spells_mr :: Table Spells_mr
spells_mr = baseTable "spells_mr"