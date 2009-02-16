
Copyright 2009 Jake Wheat

= Database utilities

Put all the database access stuff in this file, main reason is so that
the tests and other utilities can use it as well as the client
program. This has just been bodged together to get things working, so
the api design is probably a bit crap.


> module ChaosDB (

reexported HDBC stuff:

>                 Connection,
>                 handleSqlError,
>                 catchSql,

connection wrapper

>                 withConn,

Select functions

>                 selectValueIf,
>                 selectTupleIf,
>                 selectValue,
>                 selectTuple,
>                 selectTuples,
>                 selectSingleColumn,
>                 selectRelation,
>                 selectRelationValues,
>                 selectLookup,

update function

>                 dbAction,

hack functions, for calling a non action sp, and for running inserts
and updates

>                 runSql,
>                 callSp) where
>

> import Database.HDBC.PostgreSQL
> import Database.HDBC

> import Data.List
> import qualified Data.Map as M
> import Control.Monad
> import Data.Maybe
> import Control.Exception
> import Utils hiding (run)

> type SqlRow = M.Map String SqlValue
> type SelectCallback = (String -> String) -> IO ()

> withConn cs = bracket (connectPostgreSQL cs) disconnect

== notify listeners

addRelvarListner: if any of the relvars changes, then call the
supplied callback this requires a notify being setup on each table in
postgresql with the same name as the table (done automatically in the
sql)

 > addRelvarListener :: Connection -> [String] -> (IO ()) -> IO ()
 > addRelvarListener conn relvars callback = do
 >

TODO this needs writing. HDBC doesn't support it yet, not sure whether
it should use threads or what

== Query shortcuts

There are three query shortcuts,
selectValue: reading 0 or 1 values (from a relation with 0 or 1 tuples
and onne attribute)
selectTuple: reading 0 or 1 tuples
selectTuples: reading 0 to many tuples

each shortcut expects a callback function to call with value for
selectValue or with a lookup function (String->String) for
selectTuple(s)

The reason for this is a lot of the text code wants to write out some
stuff conditionally on a query not returning an empty relation, this
way seems to be reasonably concise (started to use Maybes and cases
but it was a real mess).

=== select value

runs the query, if there is a tuple returned, runs a supplied callback
with the value extracted from the query, the value is a SqlValue (this
is inconsistent with the other two select shortcuts)

errors if there isn't onne attribute or if more than one tuples are
returned from the query

the if variant doesn't error if no rows are found, they just don't
call the callback, the non if variants need onne row and error if
there is none

> selectValueIf :: Connection -> String -> [String] -> (String -> IO())-> IO ()
> selectValueIf conn query args callback = handleSqlError $ do
>   r <- quickQuery' conn query $ map toSql args
>   case length r of
>     0 -> return ()
>     1 -> do
>       let t = head r
>       when (length t /= 1)
>         (error $ "select value on " ++ query ++
>              " returned " ++ (show $ length t) ++ " attributes, expected 1.")
>       callback $ fromSql $ head t
>     _ -> error $ "select value on " ++ query ++
>              " returned " ++ (show $ length r) ++ " tuples, expected 0 or 1."

> selectValue :: Connection -> String -> IO String
> selectValue conn query = handleSqlError $ do
>   r <- quickQuery' conn query []
>   case length r of
>     0 -> error $ "select value on " ++ query ++
>              " returned 0 rows, expected 1"
>     1 -> do
>       let t = head r
>       when (length t /= 1)
>         (error $ "select value on " ++ query ++
>              " returned " ++ (show $ length t) ++ " attributes, expected 1.")
>       return $ fromSql $ head t
>     _ -> error $ "select value on " ++ query ++
>              " returned " ++ (show $ length r) ++ " tuples, expected 0 or 1."


=== select tuple

runs the query, if there is a tuple returned, convert it to a lookup
function (string->string) and call the callback with this

errors if more than one tuple is returned from the query

> selectTuple :: Connection -> String -> [String] -> SelectCallback -> IO ()
> selectTuple conn query args callback = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   case length v of
>            0 -> error $ "expected query " ++ query ++
>                 " to return 1 tuple but it returned none"
>            1 -> callback $ flip lookupAtt (convertRow cn (head v))
>            _ -> error $ "expected query " ++ query ++
>                 " to return onne tuple but it returned " ++
>                 show (length v)

> selectTupleIf :: Connection -> String -> [String] ->SelectCallback -> IO ()
> selectTupleIf conn query args callback = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   case length v of
>            0 -> return()
>            1 -> callback $ flip lookupAtt (convertRow cn (head v))
>            _ -> error ("expected query " ++ query ++
>                 " to return onne relvar but it returned " ++
>                 show (length v))

=== select tuples

just like select tuple, but instead of erroring if there is more than
one tuple, call the callback once for each tuple returned from the
query

> selectTuples :: Connection -> String -> [String] -> SelectCallback -> IO ()
> selectTuples conn query args callback = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   mapM_ callback (map (\r -> flip lookupAtt (convertRow cn r)) v)
>   return ()


> selectSingleColumn :: Connection -> String -> [String] -> IO [String]
> selectSingleColumn conn query args = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   return $ map (\r -> fromMaybe "" $ fromSql $ head r) v

> runSql :: Connection -> String -> [String] -> IO ()
> runSql conn query args = handleSqlError $ do
>   run conn query $ map toSql args
>   commit conn

=== some helpers and old stuff


==== lookupAtt

helper function used to help create the lookups that are supplied to
the callbacks in selectTuple and selectTuples. Big downside is that
all values are converted to strings, so might have to be careful with
nulls and booleans in code which uses this.

> lookupAtt :: String -> SqlRow -> String
> lookupAtt att tup =
>     case M.lookup att tup of
>       Just x -> fromMaybe "" (fromSql x)
>       Nothing -> ""


==== convert row:

It might be old fashioned but I prefer accessing database results by
the column name. (This relies on only using sql queries where all the
columns are named and named uniquely.)

> convertRow :: [String] -> [SqlValue] -> SqlRow
> convertRow cn r = M.fromList $ map
>                          (\i -> (toLower (cn !! i), (r !! i)))
>                          [0..(length cn - 1)]

==== select relation
wrote this code and never used it, kept here in case of need, will
be deleted if not used by the time the client code is completed
for the first beta release

> selectRelation :: Connection -> String -> [String] -> IO [(String->String)]
> selectRelation conn query args = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   return $ map (\r -> flip lookupAtt (convertRow cn r)) v

> selectRelationValues :: Connection -> String -> [String] -> IO [[String]]
> selectRelationValues conn query args = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   return $ for v (\r -> for r (\f -> fromSql $! f::String))

> selectLookup :: Connection -> String -> [String] -> IO [(String,String)]
> selectLookup conn query args = handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map toSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   return $ map (\r -> (fromSql (head r), fromSql (r !! 1))) v

==== callsp

shortcut to call a function in postgres hiding all the red tape you
have to go through i.e. writing the arg list as ?,?,?,...

> callSp :: Connection -> String -> [String] -> IO ()
> callSp conn spName args = handleSqlError $ do
>     let qs = intersperse ',' $ replicate (length args) '?'
>     let sqlString = "select " ++ spName ++ "(" ++ qs ++ ")"
>     quickQuery' conn sqlString $ map toSql args
>     commit conn
>     return ()

==== dbaction

call an action function in the database. not sure if this needs to be
separate from the callsp function since this code should only ever
call actions functions

> dbAction :: Connection -> String -> [String] -> IO ()
> dbAction conn actionName args = handleSqlError $ do
>   callSp conn ("action_" ++ actionName) args
>   commit conn

==== handle sql error

Tried to rewrite handleSqlError so it output newlines in pg error
messages instead of "\n" when running ghci from emacs, which makes
them very hard to read.

Doing this replacement didn't work - I don't actually know if this
problem is caused by emacs, haskell-mode, ghci, hunit or hdbc or
something else...

 > handleSqlError :: IO a -> IO a
 > handleSqlError action =
 >     catchSql action handler
 >         where handler e = do
 >                           putStrLn "\n"
 >                           putStrLn (subst "\\n" "\n" (show e))
 >                           fail ("SQL error: " ++ (subst "\\n" "\n" (show e)))
