{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Public_database_objects where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Public_database_objects =
     Record
       (HCons (LVPair Object_name (Expr (Maybe String)))
          (HCons (LVPair Object_type (Expr (Maybe String))) HNil))
 
public_database_objects :: Table Public_database_objects
public_database_objects = baseTable "public_database_objects"