{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Next_wizard where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Next_wizard =
     Record
       (HCons (LVPair Wizard_name (Expr (Maybe String)))
          (HCons (LVPair New_wizard_name (Expr (Maybe String))) HNil))
 
next_wizard :: Table Next_wizard
next_wizard = baseTable "next_wizard"