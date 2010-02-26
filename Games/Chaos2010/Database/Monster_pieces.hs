{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack51  #-}
module Games.Chaos2010.Database.Monster_pieces where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Monster_pieces =
     Record
       (HCons (LVPair Ptype (Expr (Maybe String)))
          (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
                (HCons (LVPair X (Expr (Maybe Int)))
                   (HCons (LVPair Y (Expr (Maybe Int)))
                      (HCons (LVPair Flying (Expr (Maybe Bool)))
                         (HCons (LVPair Speed (Expr (Maybe Int)))
                            (HCons (LVPair Agility (Expr (Maybe Int)))
                               (HCons (LVPair Undead (Expr (Maybe Bool)))
                                  (HCons (LVPair Ridable (Expr (Maybe Bool)))
                                     (HCons (LVPair Imaginary (Expr (Maybe Bool))) HNil)))))))))))
 
monster_pieces :: Table Monster_pieces
monster_pieces = baseTable "monster_pieces"