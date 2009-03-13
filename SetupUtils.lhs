

Copyright 2009 Jake Wheat


Purpose of this function is to script some of the game setup for a
first time player.

> module SetupUtils where

> import System.Directory
> import Control.Monad
> import System.Exit

> import Conf
> import DBAdmin


> checkSetup :: IO Conf
> checkSetup = do

If there is no config file, then create one and ask user for some info
for this.

>   f <- getConfigFilePath
>   e <- doesFileExist f
>   unless e $ do
>     a <- askYn $ "config file " ++ f ++ " doesn't exist. Shall I create it?"
>     unless a $ exitWith $ ExitFailure 1
>     putStrLn $ "creating " ++ f
>     putStrLn "Please enter the postgresql username to use"
>     un <- getLine
>     putStrLn "Please enter the postgresql password to use"
>     pw <- getLine
>     writeFile f $ "tempDbName=chaos1\n\
>                 \dbName=chaos\n\
>                 \username=" ++ un ++ "\n\
>                 \password=" ++ pw ++ "\n"
>     putStrLn "Config file written. If you need to change anything \
>               \you can edit this file in a text editor or if you \
>               \delete it and run this program again it can recreate."

Check the db login, try to connect to the database

If the database isn't present, offer to create it.

>   conf <- Conf.getConfig

>   cldb <- checkLoginAndDb conf (dbName conf)
>   case cldb of
>     LoginBad -> do
>                 putStrLn "couldn't login to dbms, please check the \
>                          \username and password in the config file"
>                 exitWith $ ExitFailure 1
>     OtherError -> do
>                 putStrLn "something went wrong"
>                 exitWith $ ExitFailure 1
>     NoDatabase -> do
>                   cr <- askYn $ "Database " ++
>                                 dbName conf ++ " doesn't exist \
>                                                \shall I create it (y/n)?"
>                   unless cr $ exitWith $ ExitFailure 1
>                   createDb conf $ dbName conf
>     OK -> return ()

If the database appears empty, offer to set it up automatically.

>   c <- getCount conf
>                 ("select count(1) from pg_class where relnamespace =\n\
>                   \(select oid from pg_namespace where nspname = 'public')\n\
>                   \and relkind = 'r' and relname='system_implementation_objects';")
>                 (dbName conf) ("check db contents " ++ dbName conf)
>   when (c == 0) $ installDbTo conf $ dbName conf

If the database doesn't appear complete, offer to reset it.

>   return conf

> askYn :: String -> IO Bool
> askYn question = do
>   putStrLn question
>   let q = do
>           a <- getLine
>           case True of
>            _ | a `elem` ["y", "Y"] -> return True
>              | a `elem` ["n", "N"] -> return False
>              | otherwise -> do
>                 putStrLn "Please answer with 'y' or 'n'"
>                 q
>   q
