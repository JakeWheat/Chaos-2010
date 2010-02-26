{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Action_instructions where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Action_instructions =
     Record
       (HCons (LVPair Action (Expr (Maybe String)))
          (HCons (LVPair Help (Expr (Maybe String))) HNil))
 
action_instructions :: Table Action_instructions
action_instructions = baseTable "action_instructions"