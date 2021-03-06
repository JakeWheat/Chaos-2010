Copyright 2010 Jake Wheat

Module to load the ui and start it using gtk

In order to keep everything responsive, we fork off a second thread
which waits on the incoming gui update channel. When it gets
something, it queues it in gtk using idleAdd

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.ConcreteUI.Gtk.GtkGUI (startGtk) where
>
> import Graphics.UI.Gtk
> import Graphics.UI.Gtk.Gdk.Events
> import Control.Concurrent.Chan.Strict
> import Control.Concurrent (forkIO)
> import Control.Applicative
> import System.FilePath.Find
> import System.FilePath
>
> import Games.Chaos2010.ConcreteUI.Gtk.GtkUtils
> import Games.Chaos2010.ConcreteUI.Gtk.TextWidget
> import Games.Chaos2010.ConcreteUI.Gtk.Types
> import qualified Games.Chaos2010.UI.UITypes as U

>
> startGtk :: Chan U.Event -> Chan (String,U.WindowUpdate) -> [U.Window] -> IO()
> startGtk evChan guChan wins = do
>   _ <- unsafeInitGUIForThreadedRTS
>   sp <- loadSprites
>   r <- mapM (\w@(U.Window title _ _ _ _ _) ->
>              (title,) <$> createWindow sp (sendEvent evChan) w) wins
>   _ <- forkIO $ readUpdates r guChan
>   mainGUI
>
> readUpdates :: [(String, ([U.MyTextItem] -> IO()))] -> Chan (String,U.WindowUpdate) -> IO ()
> readUpdates mp guChan =
>   let loop = do
>              x <- readChan guChan
>              case x of
>                (w,U.WUText u) ->
>                  case lookup w mp of
>                    Just r1 -> do
>                        _ <- flip idleAdd priorityDefaultIdle $ do
>                                     putStrLn ("Update " ++ w)
>                                     r1 u
>                                     return False
>                        return ()
>                    _ -> return ()
>                _ -> return ()
>              loop
>   in loop
>
> createWindow :: SpriteMap -> (U.Event -> IO ()) -> U.Window -> IO ([U.MyTextItem] -> IO())
> createWindow sp cb (U.Window title _ x y w h) = do
>   (tv, r) <- myTextViewNew sp cb
>   ww <- wrapInFullScroller tv >>= wrapInWindow title
>   widgetShowAll ww
>   windowMove ww x y
>   windowResize ww w h
>   _ <- onKeyPress ww $ handleKeyPress cb
>   _ <- onDestroy ww mainQuit
>   return r
>
> handleKeyPress :: (U.Event -> IO ()) -> Event -> IO Bool
> handleKeyPress cb e = do
>   _ <- case e of
>               Key { eventKeyName = key } -> cb $ U.Key key
>               _ -> error "key press handler got non key event"
>   return False

> sendEvent :: Chan U.Event -> U.Event -> IO ()
> sendEvent ch e =
>     writeChan ch e


> loadSprites :: IO SpriteMap
> loadSprites = do
>   pngNames <- find always (extension ==? ".png") "data/sprites"
>   let spriteNames = map (dropExtension . snd . splitFileName) pngNames
>   spritePixbufs <- mapM pixbufNewFromFile pngNames
>   --create the mini pixbufs
>   miniSpritePixbufs <- mapM  (\p -> pixbufScaleSimple p 16 16 InterpHyper) spritePixbufs
>   --create the normal sized pixbufs
>   mediumSpritePixbufs <- mapM (\p -> pixbufScaleSimple p 32 32 InterpHyper) spritePixbufs
>   return $ zip spriteNames spritePixbufs
>            ++ zip (map ("mini-" ++) spriteNames) miniSpritePixbufs
>            ++ zip (map ("medium-" ++) spriteNames) mediumSpritePixbufs

 >   return $ M.fromList $
 >            zip spriteNames $
 >            zip3 mediumSpritePixbufs miniSpritePixbufs spriteSurfaces
