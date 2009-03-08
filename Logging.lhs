> module Logging (
>                 setupLogging
>                ,pLog
>                ) where

> import System.Log.Handler.Simple
> import System.IO
> import System.Log.Logger
> import System.Time
> import Control.Exception
> import Control.Concurrent

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
>   --t <- getClockTime
>   --tid <- myThreadId
>   --debugM "chaos.chaos.setupLogging" (show t ++ "logging"

 > pLogM :: String -> [Char] -> IO ()
 > pLogM l m = do
 >   t <- getClockTime
 >   debugM l $ (clockTimeToString t) ++ " " ++ m

> clockTimeToString :: ClockTime -> [Char]
> clockTimeToString (TOD s ps) = show s ++ ":" ++ show ps

> pLog :: String -> String -> IO c -> IO c
> pLog logName s f = bracket (do
>                           --putStrLn $ "log to " ++ logName ++ " start " ++ s
>                           t <- getClockTime
>                           tid <- myThreadId
>                           debugM logName $
>                                  clockTimeToString t ++ " " ++ show tid ++ " start " ++ s
>                           return ())
>                          (\_ -> do
>                           --putStrLn $ "log to " ++ logName ++ " end"
>                           t <- getClockTime
>                           tid <- myThreadId
>                           debugM logName $
>                                  clockTimeToString t ++ " " ++ show tid ++ " end"
>                           return ())
>                          (\_ -> f)
