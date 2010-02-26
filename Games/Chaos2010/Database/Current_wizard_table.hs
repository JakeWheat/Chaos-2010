{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Current_wizard_table where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_table =
     Record (HCons (LVPair Current_wizard (Expr String)) HNil)
 
current_wizard_table :: Table Current_wizard_table
current_wizard_table = baseTable "current_wizard_table"