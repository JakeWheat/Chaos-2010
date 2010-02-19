Copyright 2010 Jake Wheat

Module to load the ui and start it using gtk

In order to keep everything responsive, we fork off a second thread
which waits on the incoming gui update channel. When it gets
something, it queues it in gtk using idleAdd

> {-# LANGUAGE TupleSections #-}
> module Games.Chaos2010.ConcreteUI.CUI where
>
> import Control.Concurrent.Chan.Strict
> import Control.Concurrent (forkIO)
>
> import Games.Chaos2010.ConcreteUI.Gtk.GtkGUI
> import Games.Chaos2010.ConcreteUI.SDL.SDLGui
> import Games.Chaos2010.UI.UITypes

> startGUI :: Chan Event -> Chan (String,WindowUpdate) -> [Window] -> IO()
> startGUI evChan guChan wins = do
>   let gtkWindows = filter (\(Window _ wt _ _ _ _) -> tkType wt == Gtk) wins
>       sdlWindows = filter (\(Window _ wt _ _ _ _) -> tkType wt == SDL) wins
>   spareGuChan <- dupChan guChan
>   _ <- forkIO $ startSDL evChan spareGuChan sdlWindows
>   -- start gtk in the main thread - so the app exists when gtk exits
>   startGtk evChan guChan gtkWindows

> data TkType = Gtk | SDL
>             deriving Eq

> tkType :: WidgetType -> TkType
> tkType WText = Gtk
> tkType WSpriteGrid = SDL
