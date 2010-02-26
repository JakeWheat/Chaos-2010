{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack49  #-}
module Games.Chaos2010.Database.Monster_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Monster_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Attack_strength (Expr (Maybe Int)))
             (HCons (LVPair Physical_defense (Expr (Maybe Int)))
                (HCons (LVPair Magic_defense (Expr (Maybe Int)))
                   (HCons (LVPair Flying (Expr (Maybe Bool)))
                      (HCons (LVPair Speed (Expr (Maybe Int)))
                         (HCons (LVPair Agility (Expr (Maybe Int)))
                            (HCons (LVPair Undead (Expr (Maybe Bool)))
                               (HCons (LVPair Ridable (Expr (Maybe Bool))) HNil)))))))))
 
monster_prototypes :: Table Monster_prototypes
monster_prototypes = baseTable "monster_prototypes"