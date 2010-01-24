
Copyright 2009 Jake Wheat

= Overview

This module is a method of creating a list of MyTextView.Item to feed
into a text view, by using the results of a series of database
selects. The whole description is pure, and we just convert the whole
list of selects to the List MyTextView.Item in one go. See chaos.lhs
for examples of how it's used

> module Games.Chaos2010.UI.DBTextView where

> import Control.Monad
> import Data.Maybe
> import qualified Data.Map as M

> import Games.Chaos2010.Dbms.ChaosDB
> import qualified Games.Chaos2010.UI.MyTextView as T
> import qualified Games.Chaos2010.Misc.Logging as Logging

These are the elements that you create that eventually end up in the
textbuffer. For stuff which doesn't depend on the database, there is
the passthrough (Items), and then there is a wrapper for each of the
used select varieties from ChaosDB.lhs, which take a query string,
query argument list and return a list of MyTextView.Item.

> data Item
>     = Items [T.Item]
>     | SelectValueIf String [String] (String -> [T.Item])
>     | SelectTupleIf String [String] (M.Map String String -> [T.Item])
>     | SelectTuples String [String] (M.Map String String -> [T.Item])
> --     | IOI (IO [T.Item])
>     | SelectTuplesIO String [String] (M.Map String String -> IO [T.Item])

The run function takes a list of DBTextView.Item and runs all the
selects to produce a list of MyTextView.Item ready for writing to the
textview/buffer. The code uses lists of T.Item and also Maybes which
is probably not needed, could convert the Nothings to empty lists and
it would probably be clearer.

> run :: Connection -> [Item] -> IO [T.Item]
> run conn items = Logging.pLog "chaos.DBTextView.run" "" $ do
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


Stuff used to get the types straight in the case in the run function

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
