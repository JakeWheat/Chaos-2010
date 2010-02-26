{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Action_new_game_argument where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_new_game_argument =
     Record
       (HCons (LVPair Place (Expr Int))
          (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Computer_controlled (Expr Bool)) HNil)))
 
action_new_game_argument :: Table Action_new_game_argument
action_new_game_argument = baseTable "action_new_game_argument"