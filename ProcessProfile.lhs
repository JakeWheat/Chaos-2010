#!/usr/bin/env runghc

Gave up and wrote this in ruby (See processProfile.rb), because of the
total train wreck that is regexes in haskell.

> import Data.Maybe
> import System.IO
> import System.Environment
> import Text.Regex.PCRE
> import Control.Monad

> data TimeInfoType = Start | End

> data TimeInfo = String TimeInfoType Integer

name, start or end, secs since midnight

> main :: IO ()
> main = do
>   args <- getArgs
>   text <- readFile (head args)
>   mapM_ parseLine $ lines text
>   --let matches = catMaybes $ map parseLine $ lines text
>   return ()

> parseLine :: String -> IO ()
> parseLine l = do

NOTICE:  end action_new_game 2009-02-27 00:00:30.850723+00

>   let m = getAllTextMatches (l =~
>             "^NOTICE:\\s*start ([A-Za-z_0-9]+)\\s*\
>             \\\d\\d\\d\\d-\\d\\d-\\d\\d \
>             \(\\d\\d):(\\d\\d):(\\d\\d)\\.(\\d+)\\+\\d\\d\\s*") ::[[String]]
>   unless (null m) $ putStrLn $ show m

 >   Just $ head m

  getAllTextMatches ("the place" =~ "(t|e)") :: [String]