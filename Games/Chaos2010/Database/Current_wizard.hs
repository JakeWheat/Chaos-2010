{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack41  #-}
module Games.Chaos2010.Database.Current_wizard where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard =
     Record (HCons (LVPair Wizard_name (Expr (Maybe String))) HNil)
 
current_wizard :: Table Current_wizard
current_wizard = baseTable "current_wizard"