{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack48  #-}
module Games.Chaos2010.Database.Creature_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Creature_pieces =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Flying (Expr (Maybe Bool)))
                         (HCons (LVPair Speed (Expr (Maybe Int)))
                            (HCons (LVPair Agility (Expr (Maybe Int))) HNil))))))))
 
creature_pieces :: Table Creature_pieces
creature_pieces = baseTable "creature_pieces"