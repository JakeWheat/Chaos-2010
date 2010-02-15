{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack48 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Ranged_weapon_pieces where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Ranged_weapon_pieces =
    Record (HCons (LVPair Ptype (Expr (Maybe String)))
            (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
              (HCons (LVPair X (Expr (Maybe Int)))
               (HCons (LVPair Y (Expr (Maybe Int)))
                (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                 (HCons (LVPair Range (Expr (Maybe Int)))
                  (HCons (LVPair Ranged_attack_strength (Expr (Maybe Int))) HNil))))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
ranged_weapon_pieces :: Table Ranged_weapon_pieces
ranged_weapon_pieces = baseTable "ranged_weapon_pieces"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Ptype Field
---------------------------------------------------------------------------

data PtypeTag
type Ptype = Proxy PtypeTag
instance ShowLabel Ptype where showLabel _ = "ptype"

ptype :: Ptype
ptype = proxy

---------------------------------------------------------------------------
-- Allegiance Field
---------------------------------------------------------------------------

data AllegianceTag
type Allegiance = Proxy AllegianceTag
instance ShowLabel Allegiance where showLabel _ = "allegiance"

allegiance :: Allegiance
allegiance = proxy

---------------------------------------------------------------------------
-- Tag Field
---------------------------------------------------------------------------

data TagTag
type Tag = Proxy TagTag
instance ShowLabel Tag where showLabel _ = "tag"

tag :: Tag
tag = proxy

---------------------------------------------------------------------------
-- X Field
---------------------------------------------------------------------------

data XTag
type X = Proxy XTag
instance ShowLabel X where showLabel _ = "x"

x :: X
x = proxy

---------------------------------------------------------------------------
-- Y Field
---------------------------------------------------------------------------

data YTag
type Y = Proxy YTag
instance ShowLabel Y where showLabel _ = "y"

y :: Y
y = proxy

---------------------------------------------------------------------------
-- Ranged_weapon_type Field
---------------------------------------------------------------------------

data Ranged_weapon_typeTag
type Ranged_weapon_type = Proxy Ranged_weapon_typeTag
instance ShowLabel Ranged_weapon_type where
    showLabel _ = "ranged_weapon_type"

ranged_weapon_type :: Ranged_weapon_type
ranged_weapon_type = proxy

---------------------------------------------------------------------------
-- Range Field
---------------------------------------------------------------------------

data RangeTag
type Range = Proxy RangeTag
instance ShowLabel Range where showLabel _ = "range"

range :: Range
range = proxy

---------------------------------------------------------------------------
-- Ranged_attack_strength Field
---------------------------------------------------------------------------

data Ranged_attack_strengthTag
type Ranged_attack_strength = Proxy Ranged_attack_strengthTag
instance ShowLabel Ranged_attack_strength where
    showLabel _ = "ranged_attack_strength"

ranged_attack_strength :: Ranged_attack_strength
ranged_attack_strength = proxy