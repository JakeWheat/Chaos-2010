{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45     #-}
module Games.Chaos2010.Database.Action_client_new_game_argument
       where
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_client_new_game_argument =
     Record
       (HCons (LVPair Place (Expr Int))
          (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Sprite (Expr String))
                (HCons (LVPair Colour (Expr String))
                   (HCons (LVPair Computer_controlled (Expr Bool)) HNil)))))
 
action_client_new_game_argument ::
                                Table Action_client_new_game_argument
action_client_new_game_argument
  = baseTable "action_client_new_game_argument"