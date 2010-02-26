{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Attacking_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Attacking_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Attack_strength (Expr (Maybe Int))) HNil))
 
attacking_prototypes :: Table Attacking_prototypes
attacking_prototypes = baseTable "attacking_prototypes"