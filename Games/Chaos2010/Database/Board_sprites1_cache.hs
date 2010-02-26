{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack51  #-}
module Games.Chaos2010.Database.Board_sprites1_cache where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Board_sprites1_cache =
     Record
       (HCons (LVPair X (Expr (Maybe Int)))
          (HCons (LVPair Y (Expr (Maybe Int)))
             (HCons (LVPair Ptype (Expr (Maybe String)))
                (HCons (LVPair Allegiance (Expr (Maybe String)))
                   (HCons (LVPair Tag (Expr (Maybe Int)))
                      (HCons (LVPair Sprite (Expr (Maybe String)))
                         (HCons (LVPair Colour (Expr (Maybe String)))
                            (HCons (LVPair Sp (Expr (Maybe Int)))
                               (HCons (LVPair Start_tick (Expr (Maybe Int)))
                                  (HCons (LVPair Animation_speed (Expr (Maybe Int)))
                                     (HCons (LVPair Selected (Expr (Maybe Bool))) HNil)))))))))))
 
board_sprites1_cache :: Table Board_sprites1_cache
board_sprites1_cache = baseTable "board_sprites1_cache"