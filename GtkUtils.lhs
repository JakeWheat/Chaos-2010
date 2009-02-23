
Copyright 2009 Jake Wheat

Some utils for gtk, mainly to add scrollviews and windows to widgets.

> module GtkUtils (wrapInFullScroller,
>                  wrapInVerticalScroller,
>                  wrapInWindow,
>                  getParentWidget) where

> import Graphics.UI.Gtk
> import Control.Monad

> wrapInFullScroller :: WidgetClass a => a -> IO VBox
> wrapInFullScroller w = do
>   vbox2 <- vBoxNew False 0
>   scroller <- scrolledWindowNew Nothing Nothing
>   set scroller [scrolledWindowVscrollbarPolicy := PolicyAutomatic,
>                 scrolledWindowHscrollbarPolicy := PolicyAutomatic]
>   boxPackStart vbox2 scroller PackGrow 0
>   scrolledWindowAddWithViewport scroller w
>   return vbox2

> wrapInVerticalScroller :: WidgetClass a => a -> IO VBox
> wrapInVerticalScroller w = do
>   vbox2 <- vBoxNew False 0
>   scroller <- scrolledWindowNew Nothing Nothing
>   set scroller [scrolledWindowVscrollbarPolicy := PolicyAutomatic,
>                 scrolledWindowHscrollbarPolicy := PolicyNever]
>   boxPackStart vbox2 scroller PackGrow 0
>   scrolledWindowAddWithViewport scroller w
>   return vbox2

> wrapInWindow :: WidgetClass w => String -> w -> IO Window
> wrapInWindow title widget = do
>   win <- windowNew
>   windowSetTitle win title
>   containerAdd win widget
>   return win

> getParentWidget :: WidgetClass a => a -> IO Widget
> getParentWidget wid = do
>     p <- widgetGetParent wid
>     case p of
>       Just w -> getParentWidget w
>       Nothing -> return $ castToWidget wid
