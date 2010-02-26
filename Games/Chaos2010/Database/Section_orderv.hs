{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Section_orderv where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Section_orderv =
     Record
       (HCons (LVPair Section_order (Expr (Maybe Int)))
          (HCons (LVPair Spell_category (Expr (Maybe String))) HNil))
 
section_orderv :: Table Section_orderv
section_orderv = baseTable "section_orderv"