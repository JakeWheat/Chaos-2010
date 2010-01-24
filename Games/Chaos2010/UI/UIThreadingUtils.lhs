> module Games.Chaos2010.UI.UIThreadingUtils
>     (forkIt, forkItemReplace, forkItemUpdate, forkUpdate) where

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad
> import Control.Concurrent

> import Games.Chaos2010.Dbms.ChaosDB
> import Games.Chaos2010.UI.MyTextView as MyTextView
> import qualified Games.Chaos2010.UI.DBTextView as D
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.Misc.ThreadingUtils


= temporary threading stuff

> forkIt :: IO() -> IO()
> forkIt a = forkIO a >> return()

> forkItemReplace :: Forker
>                 -> Connection
>                 -> TextView
>                 -> [Char]
>                 -> [D.Item]
>                 -> IO ()
> forkItemReplace fk conn tv logger items = do
>   forkUpdate fk logger
>              (D.run conn items)
>              (\i' -> do
>                      buf <- textViewGetBuffer tv
>                      textBufferClear buf
>                      render tv $ i')

> forkItemUpdate :: Forker
>                -> Connection
>                -> TextView
>                -> [Char]
>                -> [D.Item]
>                -> IO ()
> forkItemUpdate fk conn tv logger items = do
>   forkUpdate fk logger
>              (D.run conn items)
>              (\i' -> do
>                      render tv $ i')


> forkUpdate :: Forker -> String -> IO t -> (t -> IO ()) -> IO ()
> forkUpdate (fk,done) logger prepare rnder = do
>       fk $ lg (logger ++ ".prepare") "" $ do
>         x <- prepare
>         flip idleAdd priorityDefaultIdle $ lg (logger ++ ".render") "" $ do
>              rnder x
>              done
>              return False
>         return ()
>       return ()

 >     else prepare >>= rnder

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)
