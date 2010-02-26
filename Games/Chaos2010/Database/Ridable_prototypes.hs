{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Ridable_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Ridable_prototypes =
     Record (HCons (LVPair Ptype (Expr (Maybe String))) HNil)
 
ridable_prototypes :: Table Ridable_prototypes
ridable_prototypes = baseTable "ridable_prototypes"