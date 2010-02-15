Copyright 2010 Jake Wheat

The top level description of the entire user interface, which is a
list of windows.

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.UI.ChaosUI where

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

> chaosServer :: Database -> Connection -> (Chan String) -> (Chan (String,[MyTextItem])) -> IO ()
> chaosServer db conn inChan outChan = do
>   allUpdates db >>= mapM_ (writeChan outChan)
>   let loop = do
>         k <- readChan inChan
>         putStrLn $ "key press: " ++ k
>         callSP conn "select action_key_pressed(?);" [k]
>         mapM_ (\(n,DBText r) -> do
>                  x <- r db
>                  writeChan outChan (n,x)) chaosRenders
>         u <- allUpdates db
>         putStrLn "done em"
>         mapM_ (writeChan outChan) u
>         putStrLn "chan rote"
>         loop
>   loop

> callSP :: IConnection conn => conn -> String -> [String] -> IO ()
> callSP conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> allUpdates :: Database -> IO [(String, [MyTextItem])]
> allUpdates db =
>   mapM (\(n,DBText r) -> (n,) <$> r db) chaosRenders
