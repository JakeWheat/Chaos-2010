{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spell_sprites where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_sprites =
     Record
       (HCons (LVPair Spell_name (Expr String))
          (HCons (LVPair Sprite (Expr String)) HNil))
 
spell_sprites :: Table Spell_sprites
spell_sprites = baseTable "spell_sprites"