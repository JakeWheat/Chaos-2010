Copyright 2010 Jake Wheat

The top level description of the entire user interface, which is a
list of windows.

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.UI.ChaosUI (chaosUI, chaosServer) where

> import Control.Concurrent.Chan.Strict
> import Database.HaskellDB
> import Control.Applicative
> import Database.HDBC.PostgreSQL
> import Database.HDBC


> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.UI.InfoWidget
> import Games.Chaos2010.UI.SpellBookWidget
> import Games.Chaos2010.UI.NewGameWidget
> import Games.Chaos2010.UI.BoardWidget
> import Games.Chaos2010.UI.ActionHistoryWidget
> import Games.Chaos2010.Database.New_game_widget_state

> chaosUI :: [Window]
> chaosUI = [Window "Info" 0 371 579 213
>           ,Window "Spell Book" 587 28 268 556
>           ,Window "New Game" 514 27 500 500
>           ,Window "Board" 99 28 480 320
>           ,Window "Action History" 843 28 429 556]

> chaosKeypressed :: String -> IO ()
> chaosKeypressed k = putStrLn k

> chaosRenders :: [(String, DBText)]
> chaosRenders = [("Info", infoWidget)
>                ,("Spell Book", spellBookWidget)
>                ,("New Game", newGameWidget)
>                ,("Board", boardWidget)
>                ,("Action History", actionHistoryWidget)]

> chaosServer :: Database -> Connection -> (Chan Event) -> (Chan (String,[MyTextItem])) -> IO ()
> chaosServer db conn inChan outChan = do
>   checkNewGameRelvar db conn
>   allUpdates >>= mapM_ (writeChan outChan)
>   let loop = do
>         e <- readChan inChan
>         handleEvent db conn e
>         mapM_ (\(n,DBText r) -> do
>                                 x <- r db
>                                 writeChan outChan (n,x)) chaosRenders

>         loop
>   loop
>   where
>     allUpdates =
>         mapM (\(n,DBText r) -> (n,) <$> r db) chaosRenders

> checkNewGameRelvar :: Database -> Connection -> IO ()
> checkNewGameRelvar db conn = do
>   r <- query db c
>   forM_ r $ \r ->
>     when (r # line == 0) $
>       callSP conn "select action_reset_new_game_widget_state();" []
>   where
>     c = do
>         t1 <- table new_game_widget_state
>         project $ line .=. count(t1 .!. line)
>                 .*. emptyRecord

> callSP :: IConnection conn => conn -> String -> [String] -> IO ()
> callSP conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> handleEvent :: Database -> Connection -> Event -> IO ()
> handleEvent db conn e =
>   case e of
>     Key k -> do
>              putStrLn $ "key press: " ++ k
>              callSP conn "select action_key_pressed(?);" [k]
>     ButtonClick bid -> do
>       case bid of
>         "startGame" -> callSP conn
>            "select action_client_new_game_using_new_game_widget_state();" []
>         "resetNewGameWindow" -> callSP conn
>            "select action_reset_new_game_widget_state();" []
>         "all_pieces" -> setupTestBoard bid
>         "upgraded_wizards" -> setupTestBoard bid
>         "overlapping" -> setupTestBoard bid
>         n -> putStrLn $ "WARNING: unrecognised button id: " ++ n
>     ToggleButtonGroupClick gid bid -> putStrLn $ show e
>   where
>     setupTestBoard t =
>       callSP conn "select action_setup_test_board(?);" [t]


                                            "update new_game_widget_state\n\
                                             \set state =? where line =?"
