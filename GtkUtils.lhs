
> module GtkUtils (wrapInFullScroller,
>                  wrapInVerticalScroller,
>                  wrapInWindow,
>                  getParentWidget,
>                  textBufferGetInsertIter,
>                  textBufferInsertAtCursorWithTags,
>                  textViewInsertWidgetAtCursor,
>                  textBufferInsertPixbufAtCursor,
>                  textBufferClear,
>                  textViewScrollToBottom) where

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

================================================================================

= My Text View

Make some utility functions for working with textviews


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
