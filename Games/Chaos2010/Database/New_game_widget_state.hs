{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.New_game_widget_state where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type New_game_widget_state =
     Record
       (HCons (LVPair Line (Expr Int))
          (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Sprite (Expr String))
                (HCons (LVPair Colour (Expr String))
                   (HCons (LVPair State (Expr String)) HNil)))))
 
new_game_widget_state :: Table New_game_widget_state
new_game_widget_state = baseTable "new_game_widget_state"