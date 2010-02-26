{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.System_implementation_objects where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type System_implementation_objects =
     Record
       (HCons (LVPair Object_name (Expr String))
          (HCons (LVPair Object_type (Expr String)) HNil))
 
system_implementation_objects ::
                              Table System_implementation_objects
system_implementation_objects
  = baseTable "system_implementation_objects"