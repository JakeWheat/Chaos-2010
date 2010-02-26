{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Crimes_against_nature where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Crimes_against_nature =
     Record
       (HCons (LVPair Ptype (Expr String))
          (HCons (LVPair Allegiance (Expr String))
             (HCons (LVPair Tag (Expr Int)) HNil)))
 
crimes_against_nature :: Table Crimes_against_nature
crimes_against_nature = baseTable "crimes_against_nature"