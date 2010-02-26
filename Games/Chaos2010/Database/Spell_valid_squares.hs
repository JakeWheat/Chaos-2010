{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack43  #-}
module Games.Chaos2010.Database.Spell_valid_squares where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Spell_valid_squares =
     Record
       (HCons (LVPair Valid_square_category (Expr (Maybe String)))
          (HCons (LVPair X (Expr (Maybe Int)))
             (HCons (LVPair Y (Expr (Maybe Int))) HNil)))
 
spell_valid_squares :: Table Spell_valid_squares
spell_valid_squares = baseTable "spell_valid_squares"