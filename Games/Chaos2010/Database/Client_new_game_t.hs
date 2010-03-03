{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack45  #-}
module Games.Chaos2010.Database.Client_new_game_t where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Client_new_game_t =
     Record
       (HCons (LVPair Place (Expr Int))
          (HCons (LVPair Wizard_name (Expr String))
             (HCons (LVPair Sprite (Expr String))
                (HCons (LVPair Colour (Expr String))
                   (HCons (LVPair Computer_controlled (Expr Bool)) HNil)))))
 
client_new_game_t :: Table Client_new_game_t
client_new_game_t = baseTable "client_new_game_t"