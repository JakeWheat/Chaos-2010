Copyright 2010 Jake Wheat

Module to load the ui and start it using gtk

In order to keep everything responsive, we fork off a second thread
which waits on the incoming gui update channel. When it gets
something, it queues it in gtk using idleAdd

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.GtkUI.GtkGUI where
>
> import Graphics.UI.Gtk
> import Graphics.UI.Gtk.Gdk.Events
> import Control.Concurrent.Chan.Strict
> import Control.Concurrent (forkIO)
> import Control.Applicative
>
> import Games.Chaos2010.GtkUI.GtkUtils
> import Games.Chaos2010.GtkUI.TextWidget
> import qualified Games.Chaos2010.UI.UITypes as U
>
> startGUI :: Chan String -> Chan (String,[U.MyTextItem]) -> [U.Window] -> IO()
> startGUI kpChan guChan wins = do
>   _ <- unsafeInitGUIForThreadedRTS
>   r <- mapM (\w@(U.Window title _ _ _ _) -> (title,) <$> createWindow keyPressed w) wins
>   _ <- forkIO $ readUpdates r guChan
>   mainGUI
>   where
>     keyPressed k = writeChan kpChan k
>
> readUpdates :: [(String, ([U.MyTextItem] -> IO()))] -> Chan (String,[U.MyTextItem]) -> IO ()
> readUpdates mp guChan =
>   let loop = do
>              (w,u) <- readChan guChan
>              case lookup w mp of
>                  Nothing -> return ()
>                  Just r1 -> do
>                    _ <- flip idleAdd priorityDefaultIdle $ do
>                                 putStrLn ("Update " ++ w)
>                                 r1 u
>                                 return False
>                    return ()
>              loop
>   in loop
>
> createWindow :: (String -> IO()) -> U.Window -> IO ([U.MyTextItem] -> IO())
> createWindow cb (U.Window title x y w h) = do
>   tv <- myTextViewNew
>   ww <- wrapInFullScroller tv >>= wrapInWindow title
>   widgetShowAll ww
>   windowMove ww x y
>   windowResize ww w h
>   _ <- onKeyPress ww $ handleKeyPress cb
>   _ <- onDestroy ww mainQuit
>   return $ render tv
>
> handleKeyPress :: (String -> IO ()) -> Event -> IO Bool
> handleKeyPress cb e = do
>   _ <- case e of
>               Key { eventKeyName = key } -> cb key
>               _ -> error "key press handler got non key event"
>   return False
