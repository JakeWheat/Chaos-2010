{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Sprites where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Sprites =
     Record
       (HCons (LVPair Sprite (Expr String))
          (HCons (LVPair Animation_speed (Expr Int)) HNil))
 
sprites :: Table Sprites
sprites = baseTable "sprites"