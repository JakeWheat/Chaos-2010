{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.All_module_objects where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type All_module_objects =
     Record
       (HCons (LVPair Object_name (Expr (Maybe String)))
          (HCons (LVPair Object_type (Expr (Maybe String)))
             (HCons (LVPair Module_name (Expr (Maybe String))) HNil)))
 
all_module_objects :: Table All_module_objects
all_module_objects = baseTable "all_module_objects"