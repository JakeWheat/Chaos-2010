{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Operator_source where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Operator_source =
     Record
       (HCons (LVPair Operator_name (Expr (Maybe String)))
          (HCons (LVPair Source (Expr (Maybe String))) HNil))
 
operator_source :: Table Operator_source
operator_source = baseTable "operator_source"