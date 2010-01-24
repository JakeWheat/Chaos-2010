#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Mainly a wrapper around psql, provides commands for the front end:
reset - reset the database - used during development when you've
  altered the sql files
switch - switch the temp database over
setup - used to setup the database for an end user install


> module Games.Chaos2010.Dbms.DBAdmin (reset, switchOverTempDb, setup,
>                 checkLoginAndDb, createDb, CheckDbEnum(..),
>                 getCount, installDbTo) where

> import Control.Monad
> import System.Cmd
> import System.Exit
> import Data.List

> import Games.Chaos2010.Conf as Conf
> import Games.Chaos2010.Utils
> import Paths_Chaos2010

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
>   clearDatabase conf $ dbName conf
>   installDbTo conf $ dbName conf

> data CheckDbEnum = LoginBad | NoDatabase | OtherError | OK

> checkLoginAndDb :: Conf -> String -> IO CheckDbEnum
> checkLoginAndDb conf cdbName = do
>   (ex,t) <- runStringScriptNoThrow conf cdbName "select true;"
>                        ("check login and database " ++ cdbName)
>   if ex == 0
>     then return OK
>     else
>       if isInfixOf ("database \"" ++ cdbName ++ "\" does not exist") t
>         then return NoDatabase
>         else
>           if isInfixOf "Ident authentication failed for user" t
>             then return LoginBad
>             else return OtherError


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
>   sqlFiles <- mapM getDataFileName
>                    ["system.sql"
>                    ,"server.sql"
>                    ,"client.sql"]
>   mapM_ (runSqlScript conf idbName) sqlFiles
>   tidyDB conf idbName

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
>   (ex,t) <- systemRun command message
>   case ex of
>     0 -> return $ t
>     _ -> error $ message ++ " failed with return code " ++ show ex

> systemRun :: String -> String -> IO (Int,String)
> systemRun command message = do
>   putStrLn message
>   (std,err,ex) <- run command
>   putStrLn std
>   putStrLn err
>   return (ex, std ++ err)


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
>   runStringScript conf "template1"
>             ("alter database " ++ tempDbName conf ++
>              " rename to " ++ dbName conf ++ ";")
>             ("copying new database over old database " ++
>              tempDbName conf ++ "=>" ++ dbName conf)
>   return ()

> upargs :: Conf -> String -> String
> upargs conf cdbName = " \"user=" ++ username conf ++
>                       " password=" ++ password conf ++
>                       " dbname=" ++ cdbName ++ "\" "

> runStringScript :: Conf -> String -> String -> String -> IO String
> runStringScript conf cdbName script message = do
>   systemWithCheck
>             ("psql -c \"" ++ script ++ "\"" ++
>              upargs conf cdbName) message

> runStringScriptNoThrow :: Conf -> String -> String -> String -> IO (Int,String)
> runStringScriptNoThrow conf cdbName script message = do
>   systemRun
>             ("psql -c \"" ++ script ++ "\"" ++
>              upargs conf cdbName) message


> tidyDB :: Conf -> String  -> IO()
> tidyDB _ _ = do

 >   systemWithCheck
 >            ("psql -c \"\n\
 >             \vacuum full;\n\
 >             \analyze;\n\
 >             \reindex database " ++ cdbName ++ "\"" ++
 >             upargs conf cdbName) ("doing some maintenance on " ++ cdbName)

>   return ()

> clearDatabase :: Conf -> String -> IO ()
> clearDatabase conf kdbName = do
>    runStringScript conf kdbName
>              ("drop owned by " ++ username conf ++ " cascade;")
>              ("clearing database " ++ kdbName)
>    return ()

notes for old approach to clearing database, these generate a list of drop commands:

select 'drop function if exists ' || proname || '(' || oidvectortypes(proargtypes) || ') cascade;'
from pg_proc
where pronamespace = (select oid from pg_namespace
                           where nspname = 'public')
  order by proname;

select 'drop trigger if exists ' || tgname || ' on ' || relname  || ' cascade;'
from pg_trigger
    inner join pg_class on (tgrelid = pg_class.oid)
   inner join pg_proc on (tgfoid = pg_proc.oid)
    inner join base_relvars on (relname = base_relvars.relvar_name)
    where not tgisconstraint;

select 'drop view if exists ' || viewname || ' cascade;'
    from pg_views
    where schemaname = 'public';

select 'drop domain if exists ' || typname || ' cascade;'
    from pg_type
     where typtype = 'd'
    and typnamespace =
     (select oid from pg_namespace where nspname='public');

