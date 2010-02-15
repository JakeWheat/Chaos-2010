#! /usr/bin/env runghc

> import Database.HaskellDB.HDBC.PostgreSQL
> import Control.Concurrent.Chan
> import Control.Concurrent
> import Database.HDBC.PostgreSQL
> import Database.HDBC
> import Control.Exception
>
> import Games.Chaos2010.GtkUI.GtkGUI
> import Games.Chaos2010.UI.ChaosUI
>
> main :: IO ()
> main =
>    postgresqlConnect [("dbname", "chaos")] $ \db ->
>    withConn ("dbname=chaos") $ \conn -> do
>    keyPressChan <- newChan
>    guiUpdateChan <- newChan
>    _ <- forkIO $ chaosServer db conn keyPressChan guiUpdateChan
>    startGUI keyPressChan guiUpdateChan chaosUI


> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (connectPostgreSQL cs)
>                         disconnect
>                         f
