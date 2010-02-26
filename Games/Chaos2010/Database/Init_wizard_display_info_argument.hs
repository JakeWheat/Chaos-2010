{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Init_wizard_display_info_argument
       where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Init_wizard_display_info_argument =
     Record
       (HCons (LVPair Wizard_name (Expr String))
          (HCons (LVPair Sprite (Expr String))
             (HCons (LVPair Colour (Expr String)) HNil)))
 
init_wizard_display_info_argument ::
                                  Table Init_wizard_display_info_argument
init_wizard_display_info_argument
  = baseTable "init_wizard_display_info_argument"