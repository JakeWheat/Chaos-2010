{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Object_orders where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Object_orders =
     Record
       (HCons (LVPair Object_type (Expr (Maybe String)))
          (HCons (LVPair Object_order (Expr (Maybe Int))) HNil))
 
object_orders :: Table Object_orders
object_orders = baseTable "object_orders"