{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Current_wizard_square where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Current_wizard_square =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int))) HNil))
 
current_wizard_square :: Table Current_wizard_square
current_wizard_square = baseTable "current_wizard_square"