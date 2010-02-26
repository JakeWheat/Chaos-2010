{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Wizard_sprites where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizard_sprites =
     Record
       (HCons (LVPair Wizard_name (Expr (Maybe String)))
          (HCons (LVPair Sprite (Expr (Maybe String)))
             (HCons (LVPair Colour (Expr (Maybe String))) HNil)))
 
wizard_sprites :: Table Wizard_sprites
wizard_sprites = baseTable "wizard_sprites"