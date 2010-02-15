Copyright 2010 Jake Wheat

Module to load the ui and start it using gtk

> module Games.Chaos2010.GtkUI.GtkGUI where

> import Graphics.UI.Gtk
> import Database.HaskellDB

> import Games.Chaos2010.GtkUI.GtkUtils
> import Games.Chaos2010.GtkUI.TextWidget

> import Games.Chaos2010.UI.ChaosUI
> import qualified Games.Chaos2010.UI.UITypes as U

> startGUI :: Database -> IO()
> startGUI db = do
>   _ <- unsafeInitGUIForThreadedRTS
>   mapM_ (createWindow db) chaosUI
>   mainGUI

> createWindow :: Database -> U.Window -> IO ()
> createWindow db (U.Window title x y w h r) = do
>   tv <- textViewNew
>   renderTo db tv r
>   wi <- wrapInFullScroller tv >>= wrapInWindow title
>   widgetShowAll wi
>   windowMove wi x y
>   windowResize wi w h
>   _ <- onDestroy wi mainQuit
>   return ()

> renderTo :: Database -> TextView -> U.DBText -> IO ()
> renderTo db tv (U.DBText r) =
>   r db >>= render tv
