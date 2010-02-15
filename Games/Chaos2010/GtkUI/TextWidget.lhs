Copyright 2010 Jake Wheat

> module Games.Chaos2010.GtkUI.TextWidget
>     (myTextViewNew) where

> import Graphics.UI.Gtk
> import Control.Monad
> import Numeric
> import Data.Maybe

> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.GtkUI.Types

> myTextViewNew :: SpriteMap -> IO (TextView, [MyTextItem] -> IO())
> myTextViewNew sp = do
>   tv <- textViewNew
>   tb <- textViewGetBuffer tv
>   textBufferInsertAtCursor tb "loading..."
>   textViewSetEditable tv False
>   textViewSetWrapMode tv WrapWord
>   fd <- fontDescriptionNew
>   fontDescriptionSetSize fd 12
>   fontDescriptionSetWeight fd WeightNormal
>   widgetModifyFont tv (Just fd)
>   widgetModifyText tv StateNormal (Color 0xcfff 0xcfff 0xcfff)
>   widgetModifyBase tv StateNormal (Color 0 0 0)

Setup the tags, we want one tag for each colour in the list of colours
passed to the ctor and one tag for the inverted colour (black text on
coloured background)

>   tagTable <- textBufferGetTagTable tb
>   forM_ colours $ \(name, c) -> do
>     tag <- textTagNew (Just name)
>     set tag [textTagForeground := colourToHex c]
>     textTagTableAdd tagTable tag
>     inverseTag <- textTagNew (Just ("inverse-" ++ name))
>     set inverseTag [textTagBackground := colourToHex c,
>                     textTagForeground := "black"]
>     textTagTableAdd tagTable inverseTag
>     return ()
>   return (tv, render sp tv)
>
> colours :: [(String, Color)]
> colours = [("grid", Color 32767 32767 32767)
>           ,("background", Color 0 0 32767)
>           ,("black", Color 0 0 0)
>           ,("blue", Color 0 0 65535)
>           ,("green", Color 0 65535 0)
>           ,("red", Color 65535 0 0)
>           ,("pink", Color 65535 49407 49407)
>           ,("purple", Color 65535 0 65535)
>           ,("cyan", Color 0 65535 65535)
>           ,("yellow", Color 65535 65535 0)
>           ,("orange", Color 65535 41215 0)
>           ,("grey", Color 32767 32767 32767)
>           ,("white", Color 65535 65535 65535)]
>
> colourToHex :: Color -> String
> colourToHex (Color red green blue) =
>           "#" ++ intToHex red ++ intToHex green ++ intToHex blue
>           where
>             intToHex i =
>                 let h = showHex ((div256 i)::Int) ""
>                 in if length h < 2
>                      then '0' : h
>                      else h
>             div256 i = truncate (fromIntegral i / 256::Double)
>
> render :: SpriteMap -> TextView -> [MyTextItem] -> IO()
> render sp tv is = do
>   tb <- textViewGetBuffer tv
>   clear tb
>   mapM_ (renderItem sp tb) is
>   return ()
>   where
>     clear tb = do
>                si <- textBufferGetStartIter tb
>                ei <- textBufferGetEndIter tb
>                textBufferDelete tb si ei
>
> renderItem :: SpriteMap -> TextBuffer -> MyTextItem -> IO()
> renderItem _ tb (Text t) = do
>   textBufferInsertAtCursor tb t
>   return ()
> renderItem _ tb (TaggedText ts t) = do
>   textBufferInsertAtCursorWithTags tb ts t
>   return ()
> renderItem sp tb (Image t) = do
>   let spr = lookup t sp
>   case spr of
>     Nothing -> putStrLn $ "WARNING: pixbuf not found: " ++ t
>     Just p -> textBufferInsertPixbufAtCursor tb p
> renderItem _ _ _ = return ()


> textBufferInsertPixbufAtCursor :: TextBuffer -> Pixbuf -> IO ()
> textBufferInsertPixbufAtCursor tb pb = do
>   iter <- textBufferGetInsertIter tb
>   textBufferInsertPixbuf tb iter pb


 > data MyTextItem = Text String
 >                 | TaggedText String [String]
 >                 | Image String
 >                 | ToggleButton String Bool ToggleButtonCallback
 >                 | ToggleButtonGroup [String] String ToggleGroupCallback
 >                 | Button String ButtonCallback

 > type ToggleButtonCallback = Bool -> IO()
 > type ToggleGroupCallback = String -> IO()
 > type ButtonCallback = IO()


> textBufferInsertAtCursorWithTags :: TextBuffer -> [String] -> String -> IO ()
> textBufferInsertAtCursorWithTags tb tags text = do
>   iter <- textBufferGetInsertIter tb
>   startOffset <- textIterGetOffset iter
>   textBufferInsertAtCursor tb text
>   iter1 <- textBufferGetInsertIter tb
>   startIter <- textBufferGetIterAtOffset tb startOffset
>   tagTable <- textBufferGetTagTable tb
>   forM_ tags (\tagName -> do
>     tag <-  textTagTableLookup tagTable tagName
>     when (isJust tag) $ do
>       textBufferApplyTag tb (fromJust tag) startIter iter1)
>   return ()
>
> textBufferGetInsertIter :: TextBuffer -> IO TextIter
> textBufferGetInsertIter tb = do
> --  buf <- textViewGetBuffer tv
>   mark <- textBufferGetMark tb "insert"
>   case mark of
>     Just m -> textBufferGetIterAtMark tb m
>     Nothing -> error "couldn't find insert mark in buffer"
