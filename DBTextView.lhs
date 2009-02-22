
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
> import qualified Data.Map as M

> data Item
>     = Items [T.Item]
>     | SelectValueIf String [String] (String -> [T.Item])
>     | SelectTupleIf String [String] (M.Map String String -> [T.Item])
>     | SelectTuples String [String] (M.Map String String -> [T.Item])
> --     | IOI (IO [T.Item])
>     | SelectTuplesIO String [String] (M.Map String String -> IO [T.Item])

 > convI :: Item -> IO (Maybe [T.Item])
 > convI (Items l) = return $ Just $ l

 > convS (SelectValueIf query args callback) =
 >        makeSelectValueIf conn query args callback

 > convT query args callback =
 >        selectTupleIfC conn query args callback

 > convIO (IOI a) = do
 >   x <- a
 >   return $ Just x

 > conn :: Connection
 > conn = undefined

 > convTs (SelectTuplesIf query args callback) = do
 >   x <- makeSelectTuples conn query args callback
 >   return $ Just x



> run :: Connection -> [Item] -> IO [T.Item]
> run conn items = do
>   x <- (mapM runIt items)
>   return $ concat $ catMaybes x
>     where runIt i = case i of
>                            Items l -> return $ Just l
>                            SelectValueIf query args callback ->
>                                selectValueIfC conn query args callback
>                            SelectTuples query args callback -> do
>                                x <- selectTuplesC conn query args callback
>                                return $ Just $ concat x
>                            SelectTupleIf query args callback ->
>                                selectTupleIfC conn query args callback
>                            SelectTuplesIO query args callback -> do
>                                x <- selectTuplesIO conn query args callback
>                                return $ Just $ concat x

 >                            IOI a -> do
 >                                x <- a
 >                                return $ Just x
