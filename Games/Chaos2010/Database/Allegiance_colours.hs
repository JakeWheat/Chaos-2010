{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Allegiance_colours where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Allegiance_colours =
     Record
       (HCons (LVPair Allegiance (Expr (Maybe String)))
          (HCons (LVPair Colour (Expr (Maybe String))) HNil))
 
allegiance_colours :: Table Allegiance_colours
allegiance_colours = baseTable "allegiance_colours"