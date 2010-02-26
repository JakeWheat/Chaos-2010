{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Wizards_in_trees where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizards_in_trees =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int))) HNil)))
 
wizards_in_trees :: Table Wizards_in_trees
wizards_in_trees = baseTable "wizards_in_trees"