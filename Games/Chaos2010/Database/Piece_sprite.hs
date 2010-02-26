{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack47  #-}
module Games.Chaos2010.Database.Piece_sprite where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Piece_sprite =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Ptype (Expr (Maybe String)))
                (HCons (LVPair Sprite (Expr (Maybe String)))
                   (HCons (LVPair Colour (Expr (Maybe String)))
                      (HCons (LVPair Tag (Expr (Maybe Int)))
                         (HCons (LVPair Allegiance (Expr (Maybe String))) HNil)))))))
 
piece_sprite :: Table Piece_sprite
piece_sprite = baseTable "piece_sprite"