Copyright 2010 Jake Wheat

> module Games.Chaos2010.GtkUI.TextWidget(render) where

> import Graphics.UI.Gtk

> import Games.Chaos2010.UI.UITypes

> render :: TextView -> [MyTextItem] -> IO()
> render tv is = do
>   tb <- textViewGetBuffer tv
>   clear tb
>   mapM_ (renderItem tb) is
>   return ()
>   where
>     clear tb = do
>                si <- textBufferGetStartIter tb
>                ei <- textBufferGetEndIter tb
>                textBufferDelete tb si ei

> renderItem :: TextBuffer -> MyTextItem -> IO()
> renderItem tb (Text t) = do
>   textBufferInsertAtCursor tb t
>   return ()
> renderItem _ _ = return ()


 > data MyTextItem = Text String
 >                 | TaggedText String [String]
 >                 | Image String
 >                 | ToggleButton String Bool ToggleButtonCallback
 >                 | ToggleButtonGroup [String] String ToggleGroupCallback
 >                 | Button String ButtonCallback

 > type ToggleButtonCallback = Bool -> IO()
 > type ToggleGroupCallback = String -> IO()
 > type ButtonCallback = IO()
