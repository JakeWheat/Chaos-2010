{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Key_control_settings where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Key_control_settings =
     Record
       (HCons (LVPair Key_code (Expr String))
          (HCons (LVPair Action_name (Expr String)) HNil))
 
key_control_settings :: Table Key_control_settings
key_control_settings = baseTable "key_control_settings"