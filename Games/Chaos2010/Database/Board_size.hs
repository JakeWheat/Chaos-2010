{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42  #-}
module Games.Chaos2010.Database.Board_size where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Board_size =
     Record
       (HCons (LVPair Width (Expr Int))
          (HCons (LVPair Height (Expr Int)) HNil))
 
board_size :: Table Board_size
board_size = baseTable "board_size"