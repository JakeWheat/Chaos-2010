
> module Games.Chaos2010.UI.SetupUI (setupGui) where

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad

> import Games.Chaos2010.Dbms.ChaosDB
> import Games.Chaos2010.Utils
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.ChaosTypes
> import Games.Chaos2010.Misc.ThreadingUtils
> import Games.Chaos2010.UI.Widgets


> import Graphics.UI.Gtk hiding (disconnect)
> import Graphics.Rendering.Cairo
> import Graphics.UI.Gtk.Gdk.Events
>
> import Data.List
> import qualified Data.Map as M
> import qualified Data.Char as DC
> import Control.Monad
> import System.FilePath
> import Text.Regex.Posix
> import Control.Concurrent

> import Games.Chaos2010.Dbms.ChaosDB
> import Games.Chaos2010.UI.GtkUtils
> import Games.Chaos2010.Utils
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.UI.SoundLib
> import Games.Chaos2010.UI.Widgets
> import Games.Chaos2010.ChaosTypes
> import Games.Chaos2010.Misc.ThreadingUtils
> import Paths_Chaos2010


================================================================================

= setup gui

This code creates the widgets and windows

> setupGui :: Connection -> IO ()
> setupGui conn = lg "setupGui" "" $ handleSqlError $ do
>   colours <- readColours conn
>   spriteMap <- loadSprites conn
>   player <- initPlayer

>   aiQF <- forkOneAtATimeNew

make sure the windows relvar is ok

>   c <- selectValue conn "select count(*) from windows\n\
>                         \where window_name = 'window_manager'" []
>   when (read c == (0::Int))
>        (dbAction conn "reset_windows" [])

create the windows and widgets

>   widgetData <- selectTuplesIO conn "select window_name,px,py,sx,sy,state\n\
>                                     \from windows" [] $
>                   \r -> do
>                         (name, (ww, wrefresh)) <- makeWindow aiQF
>                                                     colours player spriteMap
>                                                     (lk "window_name" r)
>                         showWindow r ww wrefresh
>                         return (name, (ww, wrefresh))

>   let refreshAll = mapM_ (\(_,(_,r)) -> r) widgetData

>   let (_,refreshBoard) = safeLookup "get board refresh" "board" widgetData

== Key press handling

All the widgets/windows use the same key press handler, which mainly
sends the keycodes to the database code.

>   let handleKeyPress e = Logging.pLog "chaos.chaos.windowManagerNew.\
>                                         \handleKeyPress" "" $ do
>         case e of
>            Key { eventKeyName = key } -> do
>              forkIO $ lg "windowManagerNew.handleKeyPress.forkIO" "" $ do
>                --putStrLn ("Key pressed: " ++ key)
>                dbAction conn "key_pressed" [key]
>                when (key == "F12") refreshAll

>                when (key `elem` ["Up","KP_Up","Left","KP_Left","Right",
>                                  "KP_Right","Down","KP_Down","KP_Home",
>                                  "KP_Page_Up","KP_Page_Down","KP_End"])
>                     refreshBoard

>                --putStrLn "manual refresh" >> refresh
>                --Until the notify stuff is working just do a full
>                --refresh after every action as a kludge
>                --refreshAll

 >                flip idleAdd priorityDefaultIdle $ do
 >                  refreshBoard
 >                  return False

>            _ -> error "key press handler got non key event"
>         return False

Add the handler to all the windows:

>   forM_ widgetData (\(_,(window,_)) ->
>                     onKeyPress window handleKeyPress)
>   return ()

>   where
>     queueAiUpdate (fk, done) = do
>       fk $ do
>         ai <- selectValue conn "select count(1)\n\
>                                \  from valid_activate_actions\n\
>                                \   where action = 'ai_continue'" []
>         when ((read ai::Integer) /= 0) $ do
>           --putStrLn "queue ai update"
>           threadDelay 1000000
>           --putStrLn "running ai update"
>           dbAction conn "client_ai_continue_if" []
>           return ()
>         done
>       return ()

>     showWindow r ww wrefresh = do
>       widgetShowAll ww
>       windowMove ww (read $ lk "px" r) (read $ lk "py" r)
>       windowResize ww (read $ lk "sx" r) (read $ lk "sy" r)
>       wrefresh
>     castIt (iw, r) = return (castToWidget iw, r)
>     makeWindow aifk colours player spriteMap name = do
>       (widget,wrefresh) <- case name of
>           "info" ->
>             infoWidgetNew conn colours spriteMap >>= castIt
>           "board" ->
>             boardWidgetNew conn player colours spriteMap
>                            (queueAiUpdate aifk) >>= castIt
>           "spell_book" ->
>             spellBookWidgetNew conn colours spriteMap >>= castIt
>           "new_game" ->
>             newGameWidgetNew conn colours spriteMap >>= castIt
>           "action_history" ->
>             actionHistoryWidgetNew conn colours spriteMap >>= castIt
>           _ -> error ("unrecognised window name in windows relvar: " ++ name)

wrap each widget in a window

>       ww <- wrapInFullScroller widget >>=
>                        wrapInWindow name

>       onDestroy ww mainQuit

we save a list of the windows and refresh functions so that the window
manager refresh fn can hook the toggle buttons for each window up to
that window and hook pressing F12 up to refresh all the widgets

>       return (name, (ww, wrefresh))


> readColours :: Connection -> IO ColourList
> readColours conn =
>   selectTuplesC conn "select name,red,green,blue from colours" []
>                 (\t -> ((lk "name" t),
>                         Color (read (lk "red" t))
>                               (read (lk "green" t))
>                               (read (lk "blue" t))))

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)

================================================================================

= sprite manager

load sprites loads the pngs off the disk and
creates a pixbuf and mini pixbuf for use in the
text views and a cairo surface for drawing on the board

> loadSprites :: Connection -> IO SpriteMap
> loadSprites conn = lg "loadSprites" "" $ do
>   sFolder <- getDataFileName "sprites"
>   maybeSpriteFiles <- findAllFiles sFolder
>   spriteNames <- selectSingleColumn conn "select sprite from sprites" []
>   let spriteFilenames = for spriteNames
>         (\sp ->
>           let spritefiles = (filter
>                 (\l -> takeFileName l =~ ("^" ++ sp ++ "\\.[0-9]\\.png"))
>                 maybeSpriteFiles)
>           in if null spritefiles
>                then error $ "no sprite files for: " ++ sp
>                else sort spritefiles)
>   spritePixbufs <- lg "loadfromfiles" "" $ mapM
>                                              (\l -> mapM pixbufNewFromFile l)
>                                              spriteFilenames
>   let eachPb fn = mapM (\l -> mapM fn l) spritePixbufs
>   --create the mini pixbufs
>   miniSpritePixbufs <- lg "createMinisprites" "" $
>                          eachPb (\p -> pixbufScaleSimple p 16 16 InterpHyper)
>   --create the normal sized pixbufs
>   mediumSpritePixbufs <- lg "createNormalsprites" "" $
>                           eachPb (\p -> pixbufScaleSimple p 32 32 InterpHyper)
>   --create the cairo surfaces
>   spriteSurfaces <- lg "createSurfaces" "" $ forM spriteFilenames
>     (\l -> mapM imageSurfaceCreateFromPNG l)
>   return $ M.fromList $
>            zip spriteNames $
>            zip3 mediumSpritePixbufs miniSpritePixbufs spriteSurfaces

robbed this from haskell-cafe, seems that keeping the surfaces created
directly from the png files is a bad idea so load them, then draw them
to another surface and keep that surface around.

> imageSurfaceCreateFromPNG :: FilePath -> IO Surface
> imageSurfaceCreateFromPNG file =
>     withImageSurfaceFromPNG file $ \png -> do
>         w <- renderWith png $ imageSurfaceGetWidth png
>         h <- renderWith png $ imageSurfaceGetHeight png
>         surf <- createImageSurface FormatARGB32 w h
>         renderWith surf $ do
>             setSourceSurface png 0 0
>             paint
>         return surf

