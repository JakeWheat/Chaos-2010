{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack47  #-}
module Games.Chaos2010.Database.Creature_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Creature_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Attack_strength (Expr (Maybe Int)))
             (HCons (LVPair Physical_defense (Expr (Maybe Int)))
                (HCons (LVPair Magic_defense (Expr (Maybe Int)))
                   (HCons (LVPair Flying (Expr (Maybe Bool)))
                      (HCons (LVPair Speed (Expr (Maybe Int)))
                         (HCons (LVPair Agility (Expr (Maybe Int))) HNil)))))))
 
creature_prototypes :: Table Creature_prototypes
creature_prototypes = baseTable "creature_prototypes"