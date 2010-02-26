{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Attackable_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Attackable_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Physical_defense (Expr (Maybe Int))) HNil))
 
attackable_prototypes :: Table Attackable_prototypes
attackable_prototypes = baseTable "attackable_prototypes"