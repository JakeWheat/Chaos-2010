{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Database_constraints where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Database_constraints =
     Record
       (HCons (LVPair Constraint_name (Expr String))
          (HCons (LVPair Expression (Expr String)) HNil))
 
database_constraints :: Table Database_constraints
database_constraints = baseTable "database_constraints"