{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Prompt where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Prompt =
     Record
       (HCons (LVPair Action (Expr (Maybe String)))
          (HCons (LVPair Help (Expr (Maybe String))) HNil))
 
prompt :: Table Prompt
prompt = baseTable "prompt"