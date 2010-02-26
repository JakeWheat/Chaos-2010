{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack44  #-}
module Games.Chaos2010.Database.Prefered_targets where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Prefered_targets =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Action (Expr (Maybe String)))
                (HCons (LVPair Preference (Expr (Maybe Int))) HNil))))
 
prefered_targets :: Table Prefered_targets
prefered_targets = baseTable "prefered_targets"