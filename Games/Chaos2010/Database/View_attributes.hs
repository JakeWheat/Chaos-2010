{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.View_attributes where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type View_attributes =
     Record
       (HCons (LVPair Attribute_name (Expr (Maybe String)))
          (HCons (LVPair Type_name (Expr (Maybe String)))
             (HCons (LVPair Relvar_name (Expr (Maybe String))) HNil)))
 
view_attributes :: Table View_attributes
view_attributes = baseTable "view_attributes"