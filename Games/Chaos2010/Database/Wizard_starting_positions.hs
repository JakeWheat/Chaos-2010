{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44  #-}
module Games.Chaos2010.Database.Wizard_starting_positions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_starting_positions =
     Record
       (HCons (LVPair Wizard_count (Expr Int))
          (HCons (LVPair Place (Expr Int))
             (HCons (LVPair X (Expr Int)) (HCons (LVPair Y (Expr Int)) HNil))))
 
wizard_starting_positions :: Table Wizard_starting_positions
wizard_starting_positions = baseTable "wizard_starting_positions"