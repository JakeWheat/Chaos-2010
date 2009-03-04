> module Logging (
>                 setupLogging
>                ,pLog
>                ) where

> import System.Log.Handler.Simple
> import System.IO
> import System.Log.Logger
> import System.Time
> import Control.Exception

> setupLogging :: IO ()
> setupLogging = do
>   --find some way to never spit logs out on stderr
>   nullHandler <- fileHandler "/dev/null" ERROR
>   updateGlobalLogger rootLoggerName
>                (setLevel ERROR . setHandlers [nullHandler])
>   --set log level for the chaos stuff, and send to a file
>   --logger <- fileHandler "chaos.log" DEBUG
>   h <- openFile "chaos.log" AppendMode
>   logger <- verboseStreamHandler h DEBUG
>   updateGlobalLogger "chaos"
>                (setLevel DEBUG . setHandlers [logger])
>   debugM "chaos.chaos.setupLogging" "start logging"

 > pLogM :: String -> [Char] -> IO ()
 > pLogM l m = do
 >   t <- getClockTime
 >   debugM l $ (clockTimeToString t) ++ " " ++ m

> clockTimeToString :: ClockTime -> [Char]
> clockTimeToString (TOD s ps) = show s ++ "." ++ show ps

> pLog :: String -> String -> IO c -> IO c
> pLog logName s f = bracket (do
>                           --putStrLn $ "log to " ++ logName ++ " start " ++ s
>                           t <- getClockTime
>                           debugM logName $
>                                  clockTimeToString t ++ " start " ++ s
>                           return ())
>                          (\_ -> do
>                           --putStrLn $ "log to " ++ logName ++ " end"
>                           t <- getClockTime
>                           debugM logName $
>                                  clockTimeToString t ++ " end"
>                           return ())
>                          (\_ -> f)