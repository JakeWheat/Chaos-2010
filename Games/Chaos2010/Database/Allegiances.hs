{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Allegiances where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Allegiances =
     Record (HCons (LVPair Allegiance (Expr (Maybe String))) HNil)
 
allegiances :: Table Allegiances
allegiances = baseTable "allegiances"