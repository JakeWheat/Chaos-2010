
Copyright 2009 Jake Wheat

= My Text View

Make some utility functions for working with textviews, first a series
of shortcuts for adding stuff to the a textbuffer, and then a higher
level description of the contents of a buffer to allow creating
textbuffer contents using pure/ non-imperative code.

> module Games.Chaos2010.UI.MyTextView where

> import Graphics.UI.Gtk
> import Control.Monad
> import Data.Maybe

> import Games.Chaos2010.Utils
> import qualified Games.Chaos2010.Misc.Logging as Logging

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
>     when (isJust tag) $ do
>       textBufferApplyTag tb (fromJust tag) startIter iter1)
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
> textViewScrollToBottom _ = do

can't get this to actually work, have no idea if i'm using it right or
there is a bug in gtk2hs or gtk or what

 >   flip idleAdd priorityDefaultIdle $ do

 >     putStrLn "scroll to bottom"
 >     tb <- textViewGetBuffer tv
 >     ei <- textBufferGetEndIter tb
 >     o <- textIterGetLineOffset ei
 >     t <- get tb textBufferText
 >     putStrLn $ "chars in this fucking thing: " ++ (show $ length t)
 >     --putStrLn $ "scrolling to " ++ show o
 >     realIter <- textBufferGetIterAtOffset tb $ length t
 >     --putStrLn $ "scrolling to " ++ show
 >     textViewScrollToIter tv realIter 0.1 Nothing


 >     m <- textBufferGetMark tb "endMark"
 >     case m of
 >       Just m' -> do
 >              textViewScrollToMark tv m' 0 Nothing
 >              i <- textBufferGetIterAtMark tb m'
 >              o <- textIterGetLineOffset i
 >              putStrLn $ "mark offset is " ++ show o
 >       Nothing -> putStrLn "no mark"
 >     return False

 >   tb <- textViewGetBuffer tv
 >   ei <- textBufferGetEndIter tb
 >   m <- textBufferCreateMark tb (Just "endMark") ei False
 >   i <- textBufferGetIterAtMark tb m
 >   o <- textIterGetLineOffset i
 >   putStrLn $ "current mark offset is " ++ show o
 >   textViewScrollToMark tv m 0 Nothing

>   return ()

> textViewAddScrollToBottom :: TextView -> IO ()
> textViewAddScrollToBottom tv = do
>   tb <- textViewGetBuffer tv
>   ei <- textBufferGetEndIter tb
>   m <- textBufferCreateMark tb (Just "endMark") ei False
>   i <- textBufferGetIterAtMark tb m
>   o <- textIterGetLineOffset i
>   --putStrLn $ "init mark offset is " ++ show o
>   return ()


haven't worked out how to do this in gtk2hs yet.

may need this line so that it scrolls to the bottom after the last
insert instead of before

>   --queue draw

attempt to get hold of the adjustments, couldn't get it working

>   -- sw' <- getParentWidget tv
>   --putStrLn $ show sw'
>   --let sw = castToScrolledWindow sw'
>   --adj <- get sw scrolledWindowVAdjustment
>   --adjupp <- get adj adjustmentUpper
>   --adjps <- get adj adjustmentPageSize
>   --set adj [adjustmentPageSize := adjupp - adjps]

>   --putStrLn "here"
>   return ()

== Pure text view contents stuff

See chaos.lhs for examples of use.

> data Item
>     = Text String
>     | TaggedText String [String]
>     | Pixbuf Pixbuf
>     | Widget Widget
>     | ToggleButton String Bool ToggleButtonCallback
>     | ToggleButtonGroup [String] String ToggleGroupCallback
>     | Button String ButtonCallback

> type ToggleButtonCallback = Bool -> IO()
> type ToggleGroupCallback = String -> IO()
> type ButtonCallback = IO()

> render :: TextView -> [Item] -> IO ()
> render tv irl = Logging.pLog "chaos.MyTextView.render" "" $ do
>   tb <- textViewGetBuffer tv
>   let renderIt i = case i of
>                      Text s -> textBufferInsertAtCursor tb s
>                      TaggedText s t ->
>                           textBufferInsertAtCursorWithTags tb s t
>                      Pixbuf p -> textBufferInsertPixbufAtCursor tb p
>                      Widget w -> textViewInsertWidgetAtCursor tv w
>                      ToggleButton s t c -> do
>                             but <- toggleButtonNewWithLabel s
>                             textViewInsertWidgetAtCursor tv but
>                             toggleButtonSetActive but t
>                             onClicked but $ toggleButtonGetActive but >>= c
>                             return ()
>                      ToggleButtonGroup ls s c ->
>                        unless (null ls) $ do
>                        --create the first button and then all the
>                        --others so we can make them into a radio
>                        --button group
>                        b1 <- radioButtonNewWithLabel $ head ls
>                        bts <- mapM (radioButtonNewWithLabelFromWidget b1) $
>                                    tail ls
>                        -- run through the buttons, set the active
>                        -- button, insert them into the text buffer
>                        -- and add the callbacks
>                        let bs = b1 : bts
>                        mapM_ (\(l,b) -> do
>                                when (s == l) $ toggleButtonSetActive b True
>                                toggleButtonSetMode b False
>                                textViewInsertWidgetAtCursor tv b
>                                onToggled b $
>                                  whenA (toggleButtonGetActive b) $ c l
>                               ) $ zip ls bs
>                        return ()
>                      Button l c -> do
>                        but <- buttonNewWithLabel l
>                        onClicked but c
>                        textViewInsertWidgetAtCursor tv but
>   mapM_ renderIt irl