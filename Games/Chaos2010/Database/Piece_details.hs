{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack73 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.Piece_details where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type Piece_details =
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
                           (HCons (LVPair Magic_defense (Expr (Maybe Int)))
                            (HCons (LVPair Wtype (Expr (Maybe String)))
                             (HCons (LVPair Wizard_name (Expr (Maybe String)))
                              (HCons (LVPair Shadow_form (Expr (Maybe Bool)))
                               (HCons (LVPair Magic_sword (Expr (Maybe Bool)))
                                (HCons (LVPair Magic_knife (Expr (Maybe Bool)))
                                 (HCons (LVPair Magic_shield (Expr (Maybe Bool)))
                                  (HCons (LVPair Magic_wings (Expr (Maybe Bool)))
                                   (HCons (LVPair Magic_armour (Expr (Maybe Bool)))
                                    (HCons (LVPair Magic_bow (Expr (Maybe Bool)))
                                     (HCons (LVPair Computer_controlled (Expr (Maybe Bool)))
                                      (HCons (LVPair Original_place (Expr (Maybe Int)))
                                       (HCons (LVPair Expired (Expr (Maybe Bool)))
                                        (HCons (LVPair Place (Expr (Maybe Int)))
                                         (HCons (LVPair Sp (Expr (Maybe Int)))
                                          (HCons (LVPair Sprite (Expr (Maybe String)))
                                           (HCons (LVPair Colour (Expr (Maybe String))) HNil)))))))))))))))))))))))))))))))))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
piece_details :: Table Piece_details
piece_details = baseTable "piece_details"

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

---------------------------------------------------------------------------
-- Wtype Field
---------------------------------------------------------------------------

data WtypeTag
type Wtype = Proxy WtypeTag
instance ShowLabel Wtype where showLabel _ = "wtype"

wtype :: Wtype
wtype = proxy

---------------------------------------------------------------------------
-- Wizard_name Field
---------------------------------------------------------------------------

data Wizard_nameTag
type Wizard_name = Proxy Wizard_nameTag
instance ShowLabel Wizard_name where showLabel _ = "wizard_name"

wizard_name :: Wizard_name
wizard_name = proxy

---------------------------------------------------------------------------
-- Shadow_form Field
---------------------------------------------------------------------------

data Shadow_formTag
type Shadow_form = Proxy Shadow_formTag
instance ShowLabel Shadow_form where showLabel _ = "shadow_form"

shadow_form :: Shadow_form
shadow_form = proxy

---------------------------------------------------------------------------
-- Magic_sword Field
---------------------------------------------------------------------------

data Magic_swordTag
type Magic_sword = Proxy Magic_swordTag
instance ShowLabel Magic_sword where showLabel _ = "magic_sword"

magic_sword :: Magic_sword
magic_sword = proxy

---------------------------------------------------------------------------
-- Magic_knife Field
---------------------------------------------------------------------------

data Magic_knifeTag
type Magic_knife = Proxy Magic_knifeTag
instance ShowLabel Magic_knife where showLabel _ = "magic_knife"

magic_knife :: Magic_knife
magic_knife = proxy

---------------------------------------------------------------------------
-- Magic_shield Field
---------------------------------------------------------------------------

data Magic_shieldTag
type Magic_shield = Proxy Magic_shieldTag
instance ShowLabel Magic_shield where showLabel _ = "magic_shield"

magic_shield :: Magic_shield
magic_shield = proxy

---------------------------------------------------------------------------
-- Magic_wings Field
---------------------------------------------------------------------------

data Magic_wingsTag
type Magic_wings = Proxy Magic_wingsTag
instance ShowLabel Magic_wings where showLabel _ = "magic_wings"

magic_wings :: Magic_wings
magic_wings = proxy

---------------------------------------------------------------------------
-- Magic_armour Field
---------------------------------------------------------------------------

data Magic_armourTag
type Magic_armour = Proxy Magic_armourTag
instance ShowLabel Magic_armour where showLabel _ = "magic_armour"

magic_armour :: Magic_armour
magic_armour = proxy

---------------------------------------------------------------------------
-- Magic_bow Field
---------------------------------------------------------------------------

data Magic_bowTag
type Magic_bow = Proxy Magic_bowTag
instance ShowLabel Magic_bow where showLabel _ = "magic_bow"

magic_bow :: Magic_bow
magic_bow = proxy

---------------------------------------------------------------------------
-- Computer_controlled Field
---------------------------------------------------------------------------

data Computer_controlledTag
type Computer_controlled = Proxy Computer_controlledTag
instance ShowLabel Computer_controlled where
    showLabel _ = "computer_controlled"

computer_controlled :: Computer_controlled
computer_controlled = proxy

---------------------------------------------------------------------------
-- Original_place Field
---------------------------------------------------------------------------

data Original_placeTag
type Original_place = Proxy Original_placeTag
instance ShowLabel Original_place where
    showLabel _ = "original_place"

original_place :: Original_place
original_place = proxy

---------------------------------------------------------------------------
-- Expired Field
---------------------------------------------------------------------------

data ExpiredTag
type Expired = Proxy ExpiredTag
instance ShowLabel Expired where showLabel _ = "expired"

expired :: Expired
expired = proxy

---------------------------------------------------------------------------
-- Place Field
---------------------------------------------------------------------------

data PlaceTag
type Place = Proxy PlaceTag
instance ShowLabel Place where showLabel _ = "place"

place :: Place
place = proxy

---------------------------------------------------------------------------
-- Sp Field
---------------------------------------------------------------------------

data SpTag
type Sp = Proxy SpTag
instance ShowLabel Sp where showLabel _ = "sp"

sp :: Sp
sp = proxy

---------------------------------------------------------------------------
-- Sprite Field
---------------------------------------------------------------------------

data SpriteTag
type Sprite = Proxy SpriteTag
instance ShowLabel Sprite where showLabel _ = "sprite"

sprite :: Sprite
sprite = proxy

---------------------------------------------------------------------------
-- Colour Field
---------------------------------------------------------------------------

data ColourTag
type Colour = Proxy ColourTag
instance ShowLabel Colour where showLabel _ = "colour"

colour :: Colour
colour = proxy
