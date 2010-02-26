{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Magic_attackable_prototypes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Magic_attackable_prototypes =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Magic_defense (Expr (Maybe Int))) HNil))
 
magic_attackable_prototypes :: Table Magic_attackable_prototypes
magic_attackable_prototypes
  = baseTable "magic_attackable_prototypes"