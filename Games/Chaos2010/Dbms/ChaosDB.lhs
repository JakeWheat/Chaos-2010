
Copyright 2009 Jake Wheat

= Database utilities

Put all the database access stuff in this file, main reason is so that
the tests and other utilities can use it as well as the client
program. This has just been bodged together to get things working, so
the api design is probably a bit crap.


> module Games.Chaos2010.Dbms.ChaosDB (

reexported HDBC stuff:

>                 Connection
>                ,handleSqlError
>                ,catchSql

connection wrapper

>                ,withConn

Select functions

>                ,selectValueC
>                ,selectValueIfC
>                ,selectValue
>                ,selectValueIf

>                ,selectTupleC
>                ,selectTupleIfC
>                ,selectTuple
>                ,selectTupleIf

>                ,selectTuples
>                ,selectTuplesC
>                ,selectTuplesIO

>                ,selectSingleColumn
>                ,selectLookup

>                ,SqlRow

update function

>                ,dbAction

hack functions, for calling a non action sp, and for running inserts
and updates directly

>                ,runSql
>                ,callSp

>                ) where
>

> import qualified Database.HDBC.PostgreSQL as Pg
> import Database.HDBC hiding (fetchAllRows')
> import qualified Database.HDBC as H
> import Data.List
> import qualified Data.Map as M
> import Control.Monad
> import Data.Maybe
> import Control.Exception

> import Games.Chaos2010.Utils hiding (run)
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.Misc.ThreadingUtils


> type Connection = (Pg.Connection,Pg.Connection,IO() -> IO())

> type SqlRow = M.Map String String


Strategy is to use one connection for updates and one for reads. The
reads all just pile in on the same connection at the same time, seems
to work. The updates are queued using a mutex so only one update can
be using the connection at once, you definitely can't use the same
connection to run two updates at the same time when using multiple
statement transactions.

 > withConn :: String -> (Connection -> IO c) -> IO c
 > withConn cs f = f cs

> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (lg "connectPostgreSQL" "" $ do
>                             a <- Pg.connectPostgreSQL cs
>                             b <- Pg.connectPostgreSQL cs
>                             c <- makeMutexer

 >                             runSql conn "update pg_settings\n\
 >                                         \  set setting=true\n\
 >                                         \  where name='log_duration'" []
 >                             runSql conn "update pg_settings\n\
 >                                         \  set setting='all'\n\
 >                                         \  where name='log_statement'" []
 >                             runSql conn "update pg_settings\n\
 >                                         \  set setting='1'\n\
 >                                         \  where name='log_min_duration_statement'" []

>                             return (a,b,c)
>                          ) (\(a,b,_) -> do
>                              disconnect a
>                              disconnect b)
>                          f


 > withConnInt :: String -> (Pg.Connection -> IO c) -> IO c
 > withConnInt cs f = bracket (lg "connectPostgreSQL" "" $
 >                             Pg.connectPostgreSQL cs) disconnect
 >                            f

--  (\conn -> do
-- >                     runSql conn "update pg_settings\n\
-- >                                 \  set setting=true\n\
-- >                                 \  where name='log_duration'" []
-- >                     runSql conn "update pg_settings\n\
-- >                                 \  set setting='all'\n\
-- >                                 \  where name='log_statement'" []
-- >                     runSql conn "update pg_settings\n\
-- >                                 \  set setting='1'\n\
-- >                                 \  where name='log_min_duration_statement'" []
-- >                     f conn)


log_min_duration_statement
update pg_settings
  set setting=true
  where name='log_duration';
update pg_settings
  set setting='all'
  where name='log_statement';

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.ChaosDB." ++ l)

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

There are three basic query shortcuts,
selectValue: reading 0 or 1 values (from a relation with 0 or 1 tuples
and onne attribute)
selectTuple: reading 0 or 1 tuples
selectTuples: reading 0 to many tuples

=== select value

runs the query, if there is a tuple returned, runs a supplied callback
with the value extracted from the query. The non callback versions use
id as the callback. after the query is run, the return value from the
callback is returned from the select function, so you can use these in
a callback style, return style or both.

errors if there isn't onne attribute or if more than one tuples are
returned from the query

the if variant doesn't error if no rows are found, they just don't
call the callback, the non if variants need onne row and error if
there is none. The if variants return maybes and the non if versions
just return the value


> selectValueC :: Connection -> String -> [String] -> (String -> b) -> IO b
> selectValueC conn query args callback = do
>   x <- selectValueInternal conn query args callback True
>   return $ fromJust x

> selectValueIfC :: Connection -> String -> [String] -> (String -> b) -> IO (Maybe b)
> selectValueIfC conn query args callback =
>   selectValueInternal conn query args callback False

> selectValue :: Connection -> String -> [String] -> IO String
> selectValue conn query args = do
>   x <- selectValueInternal conn query args id True
>   return $ fromJust x

> selectValueIf :: Connection -> String -> [String] -> IO (Maybe String)
> selectValueIf conn query args =
>   selectValueInternal conn query args id False

> selectValueInternal :: Connection  -> String -> [String] -> (String -> b) ->
>                        Bool -> IO (Maybe b)
> selectValueInternal (conn,_,_) query args callback enf =
>   lg "selectValueInternal" query $ handleSqlError $ do
>   r <- quickQuery' conn query $ map sToSql args
>   case length r of
>     0 -> if enf
>            then error $ "select value on " ++ query ++
>                         " returned 0 rows, expected 1"
>            else return Nothing
>     1 -> do
>       let t = head r
>       when (length t /= 1)
>         (error $ "select value on " ++ query ++
>              " returned " ++ (show $ length t) ++ " attributes, expected 1.")
>       let x = callback $ toS $ head t
>       return $ Just x
>     _ -> error $ "select value on " ++ query ++
>              " returned " ++ (show $ length r) ++ " tuples, expected 0 or 1."
>   where
>     toS a = (fromSql a)::String

=== select tuple

Same pattern as the select value, the value fed into the callback or
returned when no callback is supplied is a Map String String.

> selectTupleC :: Connection -> String -> [String] -> (SqlRow -> b) -> IO b
> selectTupleC conn query args callback = do
>   x <- selectTupleInternal conn query args callback True
>   return $ fromJust x

> selectTupleIfC :: Connection -> String -> [String] -> (SqlRow -> b) -> IO (Maybe b)
> selectTupleIfC conn query args callback =
>   selectTupleInternal conn query args callback False

> selectTuple :: Connection -> String -> [String] -> IO SqlRow
> selectTuple conn query args = do
>   x <- selectTupleInternal conn query args id True
>   return $ fromJust x

> selectTupleIf :: Connection -> String -> [String] -> IO (Maybe SqlRow)
> selectTupleIf conn query args =
>   selectTupleInternal conn query args id False


> selectTupleInternal :: Connection
>                     -> String
>                     -> [String]
>                     -> (SqlRow -> b)
>                     -> Bool
>                     -> IO (Maybe b)
> selectTupleInternal (conn,_,_) query args callback enf =
>   lg "selectTupleInternal" query $ handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map sToSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   case length v of
>            0 -> if enf
>                   then error $ "expected query " ++ query ++
>                                " to return 1 tuple but it returned none"
>                   else return Nothing
>            1 -> do
>                   let x = callback $ convertRow cn (head v)
>                   return $ Just x
>            _ -> error $ "expected query " ++ query ++
>                 " to return onne tuple but it returned " ++
>                 show (length v)

> sToSql :: String -> SqlValue
> sToSql s = toSql (s::String)

=== select tuples

just like select tuple, but instead of erroring if there is zero or
more than one tuple, call the callback once for each tuple returned
from the query. returns the maps or returns from the callbacks in a
list

> selectTuplesC :: Connection -> String -> [String] -> (SqlRow -> c) -> IO [c]
> selectTuplesC = selectTuplesInternal

> selectTuples :: Connection -> String -> [String] -> IO [SqlRow]
> selectTuples conn query args = selectTuplesInternal conn query args id

IO version, this allows you to pass a callback which performs io.

> selectTuplesIO :: Connection
>                -> String
>                -> [String]
>                -> (SqlRow -> IO b)
>                -> IO [b]
> selectTuplesIO (conn,_,_) query args callback =
>   lg "selectTuplesIO" query $ handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map sToSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   mapM callback (map (convertRow cn) v)

> selectTuplesInternal :: Connection
>                      -> String
>                      -> [String]
>                      -> (SqlRow -> c)
>                      -> IO [c]
> selectTuplesInternal (conn,_,_) query args callback =
>   lg "selectTuplesInternal" query $ handleSqlError $ do
>   sth <- prepare conn query
>   execute sth $ map sToSql args
>   cn <- getColumnNames sth
>   v <- fetchAllRows' sth
>   return $ map (callback . convertRow cn) v


==== selectTuples wrappers

> selectSingleColumn :: Connection -> String -> [String] -> IO [String]
> selectSingleColumn conn query args = selectTuplesC conn query args
>                                        (\m -> snd $ head $ M.toList m)

> selectLookup :: Connection -> String -> [String] -> IO [(String,String)]
> selectLookup conn query args = handleSqlError $
>   selectTuplesC conn query args (\r -> let [a,b] = take 2 $ M.toList r
>                                       in (snd a, snd b))

=== some helpers and old stuff

==== convert row:

It might be unfashionable but I prefer accessing database results by
the column name. (This relies on only using sql queries where all the
columns are named and named uniquely.) This takes the column list
returned from getColumnNames and the sqlvalue lists and returns a Map
String String, so you run it once for each tuple.

> convertRow :: [String] -> [SqlValue] -> SqlRow
> convertRow cn r = M.fromList $ map
>                          (\i -> (toLower (cn !! i),
>                                  fromMaybe "" $ fromSql (r !! i)))
>                          [0..(length cn - 1)]

==== callsp

shortcut to call a function in postgres hiding all the red tape you
have to go through i.e. writing the arg list as ?,?,?,...

> callSp :: Connection -> String -> [String] -> IO ()
> callSp (_,conn,mrun) spName args = lg "callSp" spName $ do
>     mrun $ callSpC conn spName args

> callSpC :: Pg.Connection -> String -> [String] -> IO ()
> callSpC conn spName args = do
>     let qs = intersperse ',' $ replicate (length args) '?'
>     let sqlString = "select " ++ spName ++ "(" ++ qs ++ ")"
>     quickQuery' conn sqlString $ map toSql args
>     commit conn
>     return ()


==== runSql

> runSql :: Connection -> String -> [String] -> IO ()
> runSql (_,conn,mrun) query args = lg "runSql" query $ handleSqlError $ do
>   mrun $ do
>     run conn query $ map toSql args
>     commit conn

==== dbaction

call an action function in the database. not sure if this needs to be
separate from the callsp function since this code should only ever
call actions functions

> dbAction :: Connection -> String -> [String] -> IO ()
> dbAction (_,conn,mrun) actionName args = lg "dbAction" actionName $ handleSqlError $ do
>   mrun $ do
>     callSpC conn ("action_" ++ actionName) args
>     commit conn



> fetchAllRows' :: Statement -> IO [[SqlValue]]
> fetchAllRows' s = lg "fetchAllRows'" (originalQuery s) $ handleSqlError $
>                      H.fetchAllRows' s