{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Modules where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Modules =
     Record
       (HCons (LVPair Module_name (Expr (Maybe String)))
          (HCons (LVPair Module_order (Expr Int)) HNil))
 
modules :: Table Modules
modules = baseTable "modules"