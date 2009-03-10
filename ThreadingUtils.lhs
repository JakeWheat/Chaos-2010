
> module ThreadingUtils where

> import Control.Concurrent
> import Data.IORef
> import Control.Monad
> import Data.Maybe

> type Forker = (IO () -> IO (), IO ())

> forkOneAtATimeNew :: IO Forker
> forkOneAtATimeNew = do
>   mv <- newEmptyMVar
>   let complete = do
>         tryTakeMVar mv
>         return ()
>   let forkIt act = do
>         forkIO $ do
>           s <- tryPutMVar mv ()
>           when s act
>         return ()
>   return (forkIt, complete)


> forkAndQueueOneNew :: IO Forker
> forkAndQueueOneNew = do
>   running <- newEmptyMVar
>   queued <- newEmptyMVar
>   queuedAction <- newIORef (return())
>   let complete = do
>         --if queued, then start the queued action
>         q <- tryTakeMVar queued
>         if isJust q
>           then do
>             tryTakeMVar queued
>             a <- readIORef queuedAction
>             writeIORef queuedAction (return())
>             a
>           else tryTakeMVar running >> return ()
>         return ()
>   let forkIt act = do
>         forkIO $ do
>           s <- tryPutMVar running ()
>           if s
>             then act
>             else do
>               s' <- tryPutMVar queued ()
>               when s' $ do
>                 writeIORef queuedAction act
>         return ()
>   return (forkIt, complete)
