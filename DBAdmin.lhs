#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Mainly a wrapper around psql, provides commands for the front end:
reset - reset the database - used during development when you've
  altered the sql files
switch - switch the temp database over
setup - used to setup the database for an end user install


> module DBAdmin (reset, switchOverTempDb, setup) where

> import Conf
> import Utils
> import Control.Monad
> import System.Cmd
> import System.Exit

the way the database reset works is to load the scripts into a
database called chaos1, then if this succeeds, delete database chaos
if it exists and rename chaos1. This is done so that if there is an
error loading the new database the old database won't be overwritten
and you have that one available to test whilst you fix the errors.

> reset :: Conf.Conf -> IO ()
> -- drop database if exists, then recreate from source
> reset conf = do
>   putStrLn "Resetting Database"
>   dropDbIfExists conf $ tempDbName conf
>   createDb conf $ tempDbName conf
>   installDbTo conf $ tempDbName conf
>   switchOverTempDb conf

> createDb :: Conf -> String -> IO()
> createDb conf cdbName =
>   systemWithCheck ("psql -c \"create database " ++ cdbName ++ "\"" ++
>                    upargs conf "template1")
>                   ("create tempdb: " ++ cdbName) >> return ()

> setup :: Conf.Conf -> IO ()
> -- drop database if exists, then recreate from source
> setup conf = do
>   putStrLn "Loading database"
>   d <- dbExists conf $ dbName conf
>   unless d $ do
>     let q = do
>             putStrLn $ "Database " ++
>                        dbName conf ++ " doesn't exist \
>                                  \shall I create it (y/n)?"
>             inpStr <- getLine
>             case True of
>               _ | inpStr `elem` ["y","Y"] ->
>                     createDb conf $ dbName conf
>                 | inpStr `elem` ["n","N"] ->
>                     exitWith $ ExitFailure 1
>                 | otherwise -> q
>     q
>   installDbTo conf $ dbName conf

> installDbTo :: Conf -> String -> IO()
> installDbTo conf idbName = do
>   whenA1 (getCount conf
>             "select count(1) from pg_language where lanname='plpgsql';"
>             idbName "check plpgsql")
>          (== 0) $ systemWithCheck
>             ("psql -c \"create procedural language plpgsql;\"" ++
>              upargs conf idbName)
>             ("init plpgsql " ++ idbName) >> return ()
>   --TODO: hack for line endings, if windows then convert sql
>   --to windows line endings
>   mapM_ (runSqlScript conf idbName) ["system.sql",
>                                            "server.sql",
>                                            "client.sql"]


> runSqlScript :: Conf -> String -> String -> IO ()
> runSqlScript conf tdbName script = do
>   putStrLn ("loading " ++ script)
>   ex <- system ("psql --set ON_ERROR_STOP=on" ++
>                       " --file=" ++ script ++ upargs conf tdbName)
>   case ex of
>     ExitFailure e -> error $ "psql failed with " ++ show e
>     ExitSuccess -> return ()
>   return ()

> systemWithCheck :: String -> String -> IO String
> systemWithCheck command message = do
>   putStrLn message
>   (std,err,ex) <- run command
>   putStrLn std
>   putStrLn err
>   case ex of
>     0 -> return $ std ++ err
>     _ -> error $ message ++ " failed with return code " ++ show ex

> dbExists :: Conf -> String -> IO Bool
> dbExists conf edbName = do
>   c <- getCount conf ("select count(datname) " ++
>                 "from pg_catalog.pg_database where datname='" ++
>                 edbName ++ "'") "template1" ("check exists db " ++ edbName)
>   return $ c /= 0

> dropDbIfExists :: Conf -> String -> IO ()
> dropDbIfExists conf ddbName =
>   whenA (dbExists conf ddbName) $
>         systemWithCheck
>            ("psql -c \"drop database " ++ ddbName ++ "\"" ++
>             upargs conf "template1")
>            ("dropping database " ++ ddbName) >> return()


> getCount :: Conf -> [Char] -> String -> String -> IO Int
> getCount conf query cdbName message = do
>    text <- systemWithCheck
>             ("psql -c \"" ++ query ++ "\"" ++
>              upargs conf cdbName) message
>    return $ readCount text
>    where
>      readCount text =
>          let c = lines text
>          in case trim (c !! 2) of
>            "1" -> 1
>            "0" -> 0
>            _ -> error "error getting count"

The switch command is made available on the command line to cope with
cases where the database loads error free, but switching fails due to
the old chaos database being in use (this can happen e.g. if you have
psql open or the client has crashed and left a connection
dangling). Once you've got rid of the open connections, you can just
run switch to get the new database instead of recompiling it all.

> switchOverTempDb :: Conf -> IO ()
> switchOverTempDb conf = do
>   dropDbIfExists conf $ dbName conf
>   systemWithCheck ("psql -c " ++
>                    "\"alter database " ++ tempDbName conf ++
>                    " rename to " ++ dbName conf ++ ";\"" ++
>                    upargs conf "template1")
>                    ("copying new database over old database " ++
>                     tempDbName conf ++ "=>" ++ dbName conf)
>   return ()

> upargs :: Conf -> String -> String
> upargs conf cdbName = " \"user=" ++ username conf ++
>                       " password=" ++ password conf ++
>                       " dbname=" ++ cdbName ++ "\" "

