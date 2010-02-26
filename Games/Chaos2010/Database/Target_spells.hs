{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack48  #-}
module Games.Chaos2010.Database.Target_spells where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Target_spells =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Base_chance (Expr (Maybe Int)))
             (HCons (LVPair Alignment (Expr (Maybe Int)))
                (HCons (LVPair Spell_category (Expr (Maybe String)))
                   (HCons (LVPair Description (Expr (Maybe String)))
                      (HCons (LVPair Range (Expr (Maybe Int)))
                         (HCons (LVPair Numb (Expr (Maybe Int)))
                            (HCons (LVPair Valid_square_category (Expr (Maybe String)))
                               HNil))))))))
 
target_spells :: Table Target_spells
target_spells = baseTable "target_spells"