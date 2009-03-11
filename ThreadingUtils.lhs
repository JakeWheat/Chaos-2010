
> module ThreadingUtils where

> import Control.Concurrent
> import Data.IORef
> import Control.Monad
> import Data.Maybe
> import Control.Exception

> type Forker = (IO () -> IO (), IO ())

This forks a thread, unless one is already running, in which case it
just ignores the action passed to it. It relies on the complete
function being called when the thread is completed. The complete and
forkit methods aren't threadsafe, but seems to work at the moment, to
be fixed.

e.g. this is used to fork the ai_continue function which waits a while
then runs ai_continue. We only ever want one of these waiting to run.

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

This one forks a thread, if one is already running it queues the
thread passed to it and starts it when the previous thread
completes. If there is a thread running and a thread queued, then it
just discards the action passed to it.

e.g. this is used to handle the ui updates. We don't want two threads
reading the db and updating a widget at the same time since this is a
waste, but if we discard the second thread when the first is running
the database might have been updated after the first one has read it,
and then the ui would be out of sync with the db for some time which
is bad. By queuing one thread, we minimise the chance of this
happening to being pretty unlikely (but it is still just about
possible?).

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

> makeMutexer :: IO (IO() -> IO())
> makeMutexer = do
>   running <- newEmptyMVar
>   let runit f =bracket (putMVar running () >> return())
>                         (\_ -> takeMVar running)
>                         (\_ -> f)
>   return runit


