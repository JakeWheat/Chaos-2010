{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack42 #-}
---------------------------------------------------------------------------
-- Generated by DB/Direct
---------------------------------------------------------------------------
module Games.Chaos2010.Database.History_sounds where

import Database.HaskellDB.DBLayout

---------------------------------------------------------------------------
-- Table type
---------------------------------------------------------------------------

type History_sounds =
    Record (HCons (LVPair History_name (Expr String))
            (HCons (LVPair Sound_name (Expr String)) HNil))

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
history_sounds :: Table History_sounds
history_sounds = baseTable "history_sounds"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- History_name Field
---------------------------------------------------------------------------

data History_nameTag
type History_name = Proxy History_nameTag
instance ShowLabel History_name where showLabel _ = "history_name"

history_name :: History_name
history_name = proxy

---------------------------------------------------------------------------
-- Sound_name Field
---------------------------------------------------------------------------

data Sound_nameTag
type Sound_name = Proxy Sound_nameTag
instance ShowLabel Sound_name where showLabel _ = "sound_name"

sound_name :: Sound_name
sound_name = proxy