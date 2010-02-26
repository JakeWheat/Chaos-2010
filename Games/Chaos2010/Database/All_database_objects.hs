{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.All_database_objects where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type All_database_objects =
     Record
       (HCons (LVPair Object_type (Expr (Maybe String)))
          (HCons (LVPair Object_name (Expr (Maybe String))) HNil))
 
all_database_objects :: Table All_database_objects
all_database_objects = baseTable "all_database_objects"