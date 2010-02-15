{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack46 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Attackable_pieces where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Attackable_pieces =
    Record (HCons (LVPair Ptype (Expr (Maybe String)))
            (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
              (HCons (LVPair X (Expr (Maybe Int)))
               (HCons (LVPair Y (Expr (Maybe Int)))
                (HCons (LVPair Physical_defense (Expr (Maybe Int))) HNil))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
attackable_pieces :: Table Attackable_pieces
attackable_pieces = baseTable "attackable_pieces"

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
-- Physical_defense Field
---------------------------------------------------------------------------

data Physical_defenseTag
type Physical_defense = Proxy Physical_defenseTag
instance ShowLabel Physical_defense where
    showLabel _ = "physical_defense"

physical_defense :: Physical_defense
physical_defense = proxy
