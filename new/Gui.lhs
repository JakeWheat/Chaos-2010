
wrapper around gtk to create abstract iface to gui for the game engine to use

> module Gui
>     (runGui
>     ) where

> import Control.Concurrent.MVar
> import Control.Concurrent

> import Control.Monad

> import Data.IORef

> import Graphics.UI.Gtk hiding (get,eventKeyName)
> import Graphics.UI.Gtk.Gdk.Events
> import Graphics.Rendering.Cairo

> import Input

----------------------------------------

> runGui :: Chan Input
>        -> (IORef Surface -> IO ())
>        -> IO ()
> runGui inc drawBoard = do
>      _ <- unsafeInitGUIForThreadedRTS
>      -- create threading resources
>      backBuffer <- newIORef =<< createImageSurface FormatARGB32 1200 720
>      drawRunning <- newEmptyMVar
>
>      -- create widgets
>      window <- makeWindow "Chaos 2010" 1200 720
>      frame <- frameNew
>      containerAdd window frame
>      canvas <- drawingAreaNew
>      containerAdd frame canvas
>      widgetModifyBg canvas StateNormal (Color 0 0 0)
>      -- hook up handlers
>      widgetShowAll window
>      widgetSetAppPaintable window True
>      widgetSetDoubleBuffered window False
>      _ <- onExpose canvas $ const $ do
>        drw <- widgetGetDrawWindow canvas
>        renderWithDrawable drw $ do
>          bs <- liftIO $ readIORef backBuffer
>          setSourceSurface bs 0 0
>          paint
>        return True
>      _ <- onConfigure window $ \(Configure _ _ _ w h) -> do
>             bs <- liftIO $ readIORef backBuffer
>             bw <- imageSurfaceGetWidth bs
>             bh <- imageSurfaceGetHeight bs
>             when (bw /= w || bh /= h) $ do
>               ns <- createSimilarSurface bs ContentColorAlpha w h
>               renderWith bs $ do
>                 setSourceSurface bs 0 0
>                 paint
>               writeIORef backBuffer ns
>               surfaceFinish bs
>             return True
>      _ <- onDestroy window mainQuit
>      _ <- onKeyPress window $ \e -> do
>             _ <- case e of
>                         Key { eventKeyName = key } -> writeChan inc $ IKey key
>                         _ -> error "key press handler got non key event"
>             return True
>      -- start the backbuffer drawing thread
>      _ <- forkIO $ timer window backBuffer
>                     drawBoard
>                     drawRunning
>      mainGUI

------------------------

this function queues triggers another draw to back buffer, it is
called repeatedly to animate the display

> timer :: WidgetClass w => w -> IORef Surface -> (IORef Surface -> IO ()) -> MVar () -> IO ()
> timer win bb dtbb isDrawing = do
>   x <- tryPutMVar isDrawing ()
>   when x $ do
>     _ <- forkIO $ do
>       dtbb bb
>       takeMVar isDrawing
>       bb' <- readIORef bb
>       w <- imageSurfaceGetWidth bb'
>       h <- imageSurfaceGetHeight bb'
>       postGUIAsync $ widgetQueueDrawArea win 0 0 w h
>     return ()
>   threadDelay $ 1024 * 100
>   timer win bb dtbb isDrawing

---------------------------

> makeWindow :: String -> Int -> Int -> IO Window
> makeWindow title width height = do
>   window <- windowNew
>   set window [windowTitle := title
>              ,windowDefaultWidth := width
>              ,windowDefaultHeight := height]
>   return window

