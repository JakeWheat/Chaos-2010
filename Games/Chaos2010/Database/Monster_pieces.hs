{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack51 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Monster_pieces where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Monster_pieces =
    Record (HCons (LVPair Ptype (Expr (Maybe String)))
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

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
monster_pieces :: Table Monster_pieces
monster_pieces = baseTable "monster_pieces"

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
-- Flying Field
---------------------------------------------------------------------------

data FlyingTag
type Flying = Proxy FlyingTag
instance ShowLabel Flying where showLabel _ = "flying"

flying :: Flying
flying = proxy

---------------------------------------------------------------------------
-- Speed Field
---------------------------------------------------------------------------

data SpeedTag
type Speed = Proxy SpeedTag
instance ShowLabel Speed where showLabel _ = "speed"

speed :: Speed
speed = proxy

---------------------------------------------------------------------------
-- Agility Field
---------------------------------------------------------------------------

data AgilityTag
type Agility = Proxy AgilityTag
instance ShowLabel Agility where showLabel _ = "agility"

agility :: Agility
agility = proxy

---------------------------------------------------------------------------
-- Undead Field
---------------------------------------------------------------------------

data UndeadTag
type Undead = Proxy UndeadTag
instance ShowLabel Undead where showLabel _ = "undead"

undead :: Undead
undead = proxy

---------------------------------------------------------------------------
-- Ridable Field
---------------------------------------------------------------------------

data RidableTag
type Ridable = Proxy RidableTag
instance ShowLabel Ridable where showLabel _ = "ridable"

ridable :: Ridable
ridable = proxy

---------------------------------------------------------------------------
-- Imaginary Field
---------------------------------------------------------------------------

data ImaginaryTag
type Imaginary = Proxy ImaginaryTag
instance ShowLabel Imaginary where showLabel _ = "imaginary"

imaginary :: Imaginary
imaginary = proxy
