{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack57 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Wizard_upgrade_stats where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Wizard_upgrade_stats =
    Record (HCons (LVPair Ptype (Expr (Maybe String)))
            (HCons (LVPair Allegiance (Expr (Maybe String)))
             (HCons (LVPair Tag (Expr (Maybe Int)))
              (HCons (LVPair X (Expr (Maybe Int)))
               (HCons (LVPair Y (Expr (Maybe Int)))
                (HCons (LVPair Imaginary (Expr (Maybe Bool)))
                 (HCons (LVPair Flying (Expr (Maybe Bool)))
                  (HCons (LVPair Speed (Expr (Maybe Int)))
                   (HCons (LVPair Agility (Expr (Maybe Int)))
                    (HCons (LVPair Undead (Expr (Maybe Bool)))
                     (HCons (LVPair Ridable (Expr (Maybe Bool)))
                      (HCons (LVPair Ranged_weapon_type (Expr (Maybe String)))
                       (HCons (LVPair Range (Expr (Maybe Int)))
                        (HCons (LVPair Ranged_attack_strength (Expr (Maybe Int)))
                         (HCons (LVPair Attack_strength (Expr (Maybe Int)))
                          (HCons (LVPair Physical_defense (Expr (Maybe Int)))
                           (HCons (LVPair Magic_defense (Expr (Maybe Int))) HNil)))))))))))))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
wizard_upgrade_stats :: Table Wizard_upgrade_stats
wizard_upgrade_stats = baseTable "wizard_upgrade_stats"

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
-- Imaginary Field
---------------------------------------------------------------------------

data ImaginaryTag
type Imaginary = Proxy ImaginaryTag
instance ShowLabel Imaginary where showLabel _ = "imaginary"

imaginary :: Imaginary
imaginary = proxy

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

---------------------------------------------------------------------------
-- Attack_strength Field
---------------------------------------------------------------------------

data Attack_strengthTag
type Attack_strength = Proxy Attack_strengthTag
instance ShowLabel Attack_strength where
    showLabel _ = "attack_strength"

attack_strength :: Attack_strength
attack_strength = proxy

---------------------------------------------------------------------------
-- Physical_defense Field
---------------------------------------------------------------------------

data Physical_defenseTag
type Physical_defense = Proxy Physical_defenseTag
instance ShowLabel Physical_defense where
    showLabel _ = "physical_defense"

physical_defense :: Physical_defense
physical_defense = proxy

---------------------------------------------------------------------------
-- Magic_defense Field
---------------------------------------------------------------------------

data Magic_defenseTag
type Magic_defense = Proxy Magic_defenseTag
instance ShowLabel Magic_defense where
    showLabel _ = "magic_defense"

magic_defense :: Magic_defense
magic_defense = proxy