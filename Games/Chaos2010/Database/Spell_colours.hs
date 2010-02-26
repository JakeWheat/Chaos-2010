{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Spell_colours where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_colours =
     Record
       (HCons (LVPair Spell_name (Expr (Maybe String)))
          (HCons (LVPair Colour (Expr (Maybe String))) HNil))
 
spell_colours :: Table Spell_colours
spell_colours = baseTable "spell_colours"