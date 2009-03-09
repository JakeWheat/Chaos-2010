> module Logging (
>                 setupLogging
>                ,pLog
>                ,logTime
>                ) where

> import System.Log.Handler.Simple
> import System.IO
> import System.Log.Logger
> import System.Time
> import Control.Exception
> import Control.Concurrent

> setupLogging :: IO ()
> setupLogging = do
>   h <- openFile "chaos.log" AppendMode
>   logger <- verboseStreamHandler h DEBUG
>   updateGlobalLogger rootLoggerName
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
> pLog logName s f = bracket (logTime True logName s)
>                          (\_ -> logTime False logName s)
>                          (\_ -> f)

> logTime :: Bool -> String -> [Char] -> IO ()
> logTime st logName s = do
>   t <- getClockTime
>   tid <- myThreadId
>   debugM logName $
>          clockTimeToString t ++ " " ++ show tid ++
>          if st then " start " ++ s else " end"
>   return ()

