
> module Games.Chaos2010.UI.ChaosTextView where

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Map as M
> import qualified Data.Char as DC
> import Numeric
> import Control.Monad
> import Data.Maybe

> import Games.Chaos2010.UI.MyTextView as MyTextView
> import Games.Chaos2010.ChaosTypes


Setup a text view with the styles and colours used in this app.

> myTextViewNew :: ColourList -> IO TextView
> myTextViewNew colours = do
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
>   forM_ colours (\(name, c) -> do
>     tag <- textTagNew (Just name)
>     set tag [textTagForeground := colourToHex c]
>     textTagTableAdd tagTable tag
>     inverseTag <- textTagNew (Just ("inverse-" ++ name))
>     set inverseTag [textTagBackground := colourToHex c,
>                     textTagForeground := "black"]
>     textTagTableAdd tagTable inverseTag
>     return ())

>
>   return tv


> textBufferInsertSpriteAtCursor :: TextBuffer -> String -> SpriteMap -> IO()
> textBufferInsertSpriteAtCursor tb spriteName spriteMap =
>   case M.lookup spriteName spriteMap of
>     Just (pb,_,_) -> textBufferInsertPixbufAtCursor tb (head pb)
>     _ -> return ()

> textBufferInsertMiniSpriteAtCursor :: TextBuffer ->
>                                       String ->
>                                       SpriteMap ->
>                                       IO()
> textBufferInsertMiniSpriteAtCursor tb spriteName spriteMap =
>   case M.lookup spriteName spriteMap of
>     Just (_,pb,_) -> textBufferInsertPixbufAtCursor tb (head pb)
>     _ -> return ()

== colorToHex

colourToHex: convert a GDK::Color to a html style #FFFFFF colour which
is what the textbuffer tags want to see when setting colours up, there
must be an easier way than this?

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
