{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46  #-}
module Games.Chaos2010.Database.Magic_attackable_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Magic_attackable_pieces =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Magic_defense (Expr (Maybe Int))) HNil))))))
 
magic_attackable_pieces :: Table Magic_attackable_pieces
magic_attackable_pieces = baseTable "magic_attackable_pieces"