{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Spells_mr_base where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spells_mr_base =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Base_chance (Expr (Maybe Int)))
             (HCons (LVPair Alignment (Expr (Maybe Int)))
                (HCons (LVPair Spell_category (Expr (Maybe String)))
                   (HCons (LVPair Description (Expr (Maybe String))) HNil)))))
 
spells_mr_base :: Table Spells_mr_base
spells_mr_base = baseTable "spells_mr_base"