#! /usr/bin/env runghc

> import Database.HaskellDB.HDBC.PostgreSQL
> import Control.Concurrent.Chan.Strict
> import Control.Concurrent hiding (newChan)
> import Database.HDBC.PostgreSQL
> import Database.HDBC
> import Control.Exception
>
> --import Games.Chaos2010.ConcreteUI.CUI
> import Games.Chaos2010.UI.ChaosUI
>
> main :: IO ()
> main =
>    startGui "chaos" chaosWindows

to regenerate the haskelldb code use:

DBDirect-hdbc-postgresql Games.Chaos2010.Database "dbname=chaos"
