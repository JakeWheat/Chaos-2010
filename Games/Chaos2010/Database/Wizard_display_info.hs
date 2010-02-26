{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Wizard_display_info where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_display_info =
     Record
       (HCons (LVPair Wizard_name (Expr String))
          (HCons (LVPair Default_sprite (Expr String))
             (HCons (LVPair Colour (Expr String)) HNil)))
 
wizard_display_info :: Table Wizard_display_info
wizard_display_info = baseTable "wizard_display_info"