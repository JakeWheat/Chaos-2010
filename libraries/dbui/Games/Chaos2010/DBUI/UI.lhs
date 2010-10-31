

> module Games.Chaos2010.UI
>     where
> import Database.HaskellDB
> import Games.Chaos2010.Database.Turn_number_table

> startUI :: String -> [Window] -> IO()
> startUI = undefined
>    {-postgresqlConnect [("dbname", "chaos")] $ \db ->
>    withConn ("dbname=chaos") $ \conn -> do
>    keyPressChan <- newChan
>    guiUpdateChan <- newChan
>    _ <- forkIO $ chaosServer db conn keyPressChan guiUpdateChan
>    startGUI keyPressChan guiUpdateChan chaosUI
>    return ()
>
> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (connectPostgreSQL cs)
>                         disconnect
>                         f-}

start ui is a server


> data Window = Window String Int Int Int Int [Widget]

> data StaticWidget = SpriteGrid Int Int [(Int,Int,String)]
>                   | Text String
>                   | TaggedText [String] String
>                   | Image String
>                   | ToggleButtonGroup [(String,String)] String (String -> IO()) --name,label, name of current select, callback
>                   | Button String (IO())

> data Widget = Widget -- Widget Query (Database -> StaticWidget)

> a = table turn_number_table