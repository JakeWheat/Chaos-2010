
= Overview

This module is a method of creating a TList to feed into a text view,
by using the results of a series of database selects. The whole
description is pure, and we just convert the whole list of selects to
a TList object in one go.

> module DBTextView where

> import qualified MyTextView as T
> import Control.Monad
> import ChaosDB
> import Data.Maybe

> data Item
>     = Items [T.Item]
>     | SelectValueIf String [String] (String -> [T.Item])
>     | SelectTupleIf String [String] ((String->String) -> [T.Item])

 > convI :: Item -> IO (Maybe [T.Item])
 > convI (Items l) = return $ Just $ l

 > convS (SelectValueIf query args callback) =
 >        makeSelectValueIf conn query args callback

 > convT (SelectTupleIf query args callback) =
 >        makeSelectTupleIf conn query args callback

> run :: Connection -> [Item] -> IO [T.Item]
> run conn items = do
>   x <- (mapM runIt items)
>   return $ concat $ catMaybes x
>     where runIt i = case i of
>                            Items l -> return $ Just $ l
>                            SelectValueIf query args callback ->
>                                makeSelectValueIf conn query args callback
>                            SelectTupleIf query args callback ->
>                                makeSelectTupleIf conn query args callback
