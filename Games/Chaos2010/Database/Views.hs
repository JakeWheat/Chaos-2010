{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack49  #-}
module Games.Chaos2010.Database.Views where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Views =
     Record
       (HCons (LVPair View_name (Expr (Maybe String)))
          (HCons (LVPair Table_catalog (Expr (Maybe String)))
             (HCons (LVPair Definition (Expr (Maybe String)))
                (HCons (LVPair Table_schema (Expr (Maybe String)))
                   (HCons (LVPair Table_name (Expr (Maybe String)))
                      (HCons (LVPair View_definition (Expr (Maybe String)))
                         (HCons (LVPair Check_option (Expr (Maybe String)))
                            (HCons (LVPair Is_updatable (Expr (Maybe String)))
                               (HCons (LVPair Is_insertable_into (Expr (Maybe String)))
                                  HNil)))))))))
 
views :: Table Views
views = baseTable "views"