{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack50  #-}
module Games.Chaos2010.Database.Squares_valid_categories where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Squares_valid_categories =
     Record
       (HCons (LVPair Category (Expr (Maybe String)))
          (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int)))
                (HCons (LVPair Ptype (Expr (Maybe String)))
                   (HCons (LVPair Allegiance (Expr (Maybe String)))
                      (HCons (LVPair Tag (Expr (Maybe Int)))
                         (HCons (LVPair Undead (Expr (Maybe Bool)))
                            (HCons (LVPair Ridable (Expr (Maybe Bool)))
                               (HCons (LVPair Creature (Expr (Maybe Bool)))
                                  (HCons (LVPair Monster (Expr (Maybe Bool))) HNil))))))))))
 
squares_valid_categories :: Table Squares_valid_categories
squares_valid_categories = baseTable "squares_valid_categories"