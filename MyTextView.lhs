
> module MyTextView where

> import Graphics.UI.Gtk
> import GtkUtils
> import Control.Monad
> import Data.Maybe

================================================================================

= My Text View

Make some utility functions for working with textviews

Add the TList data type for describing the contents of a text view
using pure code, this allows us to fill a text view with content
without using lots of imperative code.

== text view utils

> textBufferGetInsertIter :: TextBuffer -> IO TextIter
> textBufferGetInsertIter tb = do
> --  buf <- textViewGetBuffer tv
>   mark <- textBufferGetMark tb "insert"
>   case mark of
>     Just m -> textBufferGetIterAtMark tb m
>     Nothing -> error "couldn't find insert mark in buffer"

> textBufferInsertAtCursorWithTags :: TextBuffer -> String -> [String] -> IO ()
> textBufferInsertAtCursorWithTags tb text tags = do
>   iter <- textBufferGetInsertIter tb
>   startOffset <- textIterGetOffset iter
>   textBufferInsertAtCursor tb text
>   iter1 <- textBufferGetInsertIter tb
>   startIter <- textBufferGetIterAtOffset tb startOffset
>   tagTable <- textBufferGetTagTable tb
>   forM_ tags (\tagName -> do
>     tag <-  textTagTableLookup tagTable tagName
>     case tag of
>               Just tag' -> textBufferApplyTag tb tag' startIter iter1
>               Nothing -> error ("Didn't match tag named: " ++ tagName))
>   return ()

> textViewInsertWidgetAtCursor :: WidgetClass a => TextView -> a -> IO ()
> textViewInsertWidgetAtCursor tv w = do
>   tb <- textViewGetBuffer tv
>   iter <- textBufferGetInsertIter tb
>   anc <- textBufferCreateChildAnchor tb iter
>   textViewAddChildAtAnchor tv w anc
>   widgetShowAll w

> textBufferInsertPixbufAtCursor :: TextBuffer -> Pixbuf -> IO ()
> textBufferInsertPixbufAtCursor tb pb = do
>   iter <- textBufferGetInsertIter tb
>   textBufferInsertPixbuf tb iter pb


Shortcut method to clear buffer - i.e. wipe the textview clean to start again

> textBufferClear :: TextBuffer -> IO ()
> textBufferClear tb = do
>   si <- textBufferGetStartIter tb
>   ei <- textBufferGetEndIter tb
>   textBufferDelete tb si ei

> textViewScrollToBottom :: TextView -> IO ()
> textViewScrollToBottom tv = do

haven't worked out how to do this in gtk2hs yet.

may need this line so that it scrolls to the bottom after the last
insert instead of before

>   --queue draw

attempt to get hold of the adjustments, couldn't get it working

>   --sw' <- getParentWidget tv
>   --putStrLn $ show sw'
>   --let sw = castToScrolledWindow sw'
>   --adj <- get sw scrolledWindowVAdjustment
>   --adjupp <- get adj adjustmentUpper
>   --adjps <- get adj adjustmentPageSize
>   --set adj [adjustmentPageSize := adjupp - adjps]

>   --putStrLn "here"
>   return ()

== Pure text view contents stuff

> data Item
>     = Text String
>     | TaggedText String [String]
>     | Pixbuf Pixbuf
>     | Widget Widget
>     | ToggleButton String Bool ToggleButtonCallback

> type ToggleButtonCallback = Bool -> IO()

> render :: TextView -> [Item] -> IO ()
> render tv irl = do
>   tb <- textViewGetBuffer tv
>   let renderIt c = case c of
>                      Text s -> textBufferInsertAtCursor tb s
>                      TaggedText s t ->
>                           textBufferInsertAtCursorWithTags tb s t
>                      Pixbuf p -> textBufferInsertPixbufAtCursor tb p
>                      Widget w -> textViewInsertWidgetAtCursor tv w
>                      ToggleButton s t c -> do
>                             but <- toggleButtonNewWithLabel s
>                             textViewInsertWidgetAtCursor tv but
>                             toggleButtonSetActive but t
>                             onClicked but $ do
>                               active <- toggleButtonGetActive but
>                               c active
>                             return ()
>   mapM_ renderIt irl

