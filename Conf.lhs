
Copyright 2009 Jake Wheat

Short code to read the config file and expose values as a record.

> module Conf (getConfig, Conf (..)) where

> import Fez.Data.Conf
> import Data.Map as M
> import Data.List
> import System.IO
> import System.Directory
> import System.FilePath

> import Utils


> data Conf = Conf { tempDbName :: String,
>               dbName :: String,
>               username :: String,
>               password :: String }

> expectedKeys :: [[Char]]
> expectedKeys = sort ["tempDbName", "dbName", "username", "password"]

> getConfig :: IO (Conf)
> getConfig = do
>   cfgFolder <- getHomeDirectory
>   f <- readFile $ joinPath [cfgFolder,".chaos.config"]
>   let m = parseToMap f

>   if M.keys m /= expectedKeys
>     then error $ "Config file must contain exactly these keys: " ++
>                  show expectedKeys ++ "\ngot " ++ show (M.keys m)
>     else
>         let k key = safeMLookup "get config" key m
>         in return $ Conf (k "tempDbName")
>                      (k "dbName")
>                      (k "username")
>                      (k "password")
