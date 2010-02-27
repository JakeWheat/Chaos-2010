Copyright 2010 Jake Wheat

The top level description of the entire user interface, which is a
list of windows.

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.UI.ChaosUI (chaosUI, chaosServer) where

> import Control.Concurrent.Chan.Strict
> import Database.HaskellDB as H
> import Control.Applicative
> import Database.HDBC (IConnection)


> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.UI.InfoWidget
> import Games.Chaos2010.UI.SpellBookWidget
> import Games.Chaos2010.UI.NewGameWidget
> import Games.Chaos2010.UI.ActionHistoryWidget
> import Games.Chaos2010.UI.BoardWidget
> import Games.Chaos2010.Database.New_game_widget_state
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Database.Fields

> chaosUI :: [Window]
> chaosUI = [Window "Info" WText 0 371 579 213
>           ,Window "Spell Book" WText 587 28 268 556
>           ,Window "New Game" WText 514 27 500 500
>           ,Window "Board" WSpriteGrid 99 28 480 320
>           ,Window "Action History" WText 843 28 429 556]

> chaosRenders :: IConnection conn => conn  -> [(String, Database -> IO WindowUpdate)]
> chaosRenders conn = [("Info", unwrapDBText infoWidget)
>                     ,("Spell Book", unwrapDBText spellBookWidget)
>                     ,("New Game", unwrapDBText $ newGameWidget conn)
>                     ,("Board", unwrapSG boardWidget)
>                     ,("Action History", unwrapDBText actionHistoryWidget)]

> unwrapDBText :: DBText -> Database -> IO WindowUpdate
> unwrapDBText (DBText u) = fmap WUText . u

> unwrapSG :: DBSpriteGrid -> Database -> IO WindowUpdate
> unwrapSG (DBSpriteGrid u) = fmap WUSpriteGrid . u

> chaosServer :: IConnection conn => Database -> conn -> (Chan Event) -> (Chan (String,WindowUpdate)) -> IO ()
> chaosServer db conn inChan outChan = do
>   return ()
>   checkNewGameRelvar db conn
>   allUpdates >>= mapM_ (writeChan outChan)
>   let loop = do
>         e <- readChan inChan
>         handleEvent db conn e
>         allUpdates >>= mapM_ (writeChan outChan)
>         loop
>   loop
>   where
>     allUpdates =
>         mapM (\(n,u) -> (n,) <$> u db) $ chaosRenders conn


> checkNewGameRelvar :: IConnection conn => Database -> conn -> IO ()
> checkNewGameRelvar db conn = do
>   r <- query db c
>   forM_ r $ \rt ->
>     when (rt # line == 0) $ resetNewGameWidgetState conn
>   where
>     c = do
>         t1 <- table new_game_widget_state
>         project $ line .=. H.count(t1 .!. line)
>                 .*. emptyRecord

> handleEvent :: IConnection conn => Database -> conn -> Event -> IO ()
> handleEvent _ conn e =
>   case e of
>     Key k -> do
>              putStrLn $ "key press: " ++ k
>              --sendKeyPress conn k
>     Callback c -> c
