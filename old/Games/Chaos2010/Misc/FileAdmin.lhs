#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Some development utilities, some code to help check the sprite pngs on
disk match the sprite table in the database.

> module Games.Chaos2010.Misc.FileAdmin (checkSprites
>                   ) where

> import Data.List
> import Control.Monad
> import Text.Regex.Posix
> import System.FilePath

> import qualified Games.Chaos2010.Conf as Conf
> import Games.Chaos2010.Dbms.ChaosDB
> import Games.Chaos2010.Utils


function to check all the sprite files are present and all the
pngs under sprites are referenced by the sprite table.
todo: doesn't check all the sprite files are present

> checkSprites :: IO ()
> checkSprites = do
>   conf <- Conf.getConfig
>   spriteFiles <- withConn ("host=localhost dbname=" ++ Conf.dbName conf ++
>                    " user=" ++ Conf.username conf ++
>                    " password=" ++ Conf.password conf)
>     (\conn -> do
>        spriteNames <- selectSingleColumn conn "select sprite from sprites" []
>        maybeSpriteFiles <- findAllFiles "sprites"
>        let matchesSprite sn fn = takeFileName fn =~
>                                    ("^" ++ sn ++ "\\.[0-9]\\.png")
>            filesForSprite sn = filter (matchesSprite sn) maybeSpriteFiles
>        return $ concatMap filesForSprite spriteNames)
>   maybeSpriteFiles <- findAllFiles "sprites"
>   let orphans = maybeSpriteFiles \\ spriteFiles
>   if null orphans
>     then putStrLn "OK: no unmatched pngs"
>     else putStrLn $ "unmatched pngs:\n" ++
>            intercalate "\n" orphans
