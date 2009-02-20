#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

create an outline view of the source files
and of the database modules

For files, create a tree based on the headings in the source file: =,
==, etc. style, and add the first line of function, table, view,
etc. definitions.

For the database, start with a tree of the modules and fill each
module leaf with the functions, tables, views, etc. in that module.

Easier just to run it and see.

todo: get this tree into speedbar

> import Prelude hiding (catch)
> import GtkUtils
> import ChaosDB
> import Graphics.UI.Gtk  hiding (fill,
>                                 disconnect)
> import Data.Tree
> import Data.List
> import Data.Maybe
> import Text.Regex.Posix
> import Conf
> import System.Directory
> import Control.Monad
> import Control.Exception
> import Utils

> getSourceFiles = do
>   entries <- getDirectoryContents "."
>   filterM doesFileExist entries

Create the tree widget and do a bunch of red tape.

> main :: IO ()
> main = do
>        unsafeInitGUIForThreadedRTS
>        w <- treeThingNew
>        sc <- wrapInFullScroller w
>        win <- wrapInWindow "Tree thing" sc
>        windowResize win 1000 800
>        widgetShowAll win
>        onDestroy win mainQuit
>        mainGUI


> treeThingNew = do

create a treeview with one column for the text

>   tree <- treeViewNew
>   rend <- cellRendererTextNew
>   col <- treeViewColumnNew
>   treeViewColumnPackStart col rend True

now get the data for the tree - module trees from the database and
filetrees from the source files

>   moduleTrees <- getModuleTree
>
>   fileTrees <- getSourceFiles >>= mapM (\f -> do
>                        ft <- parseFile f
>                        return Node {
>                            rootLabel=f,
>                            subForest=ft})
>   model <- treeStoreNew $ fileTrees ++
>            [Node {rootLabel="database", subForest=moduleTrees}]

more gtk red tape to finish off the tree with one text column stuff

>   cellLayoutSetAttributes col rend model $
>                               \row -> [cellText := row]
>   treeViewAppendColumn tree col
>   treeViewSetModel tree model
>   return tree

add node takes a tree and a path and adds the path to the tree
matching the path elements to existing nodes in the tree or creating
new branches and leafs.

> addNode :: (Eq a, Show a) => [Tree a] -> [a] -> [Tree a]
> addNode tree path = case True of
>                     _ | null path ->
>                           --error "tried to add empty path"
>                           tree
>                       | null tree && length path == 1 ->
>                           --add new leaf
>                           [Node {rootLabel = head path,
>                                  subForest = []}]
>                       | null tree && length path > 1 ->
>                           [Node {rootLabel = head path,
>                                  subForest = addNode [] (tail path)}]
>                       | rootLabel (head tree) == head path ->
>                               let hd = head tree
>                                   tl = tail tree
>                               in hd {subForest =
>                                      addNode (subForest hd)
>                                              (tail path)} : tl
>                       | otherwise ->
>                           head tree : addNode (tail tree) path
>
>
>

get module tree gets the objects from the database, starting with the
modules and then adding the functions, views, scalars, triggers,
tables and constraints under the appropriate module

todo: add internal objects under an additional branch for each module

> getModuleTree :: IO [Tree [Char]]
> getModuleTree =
>   catchSql (do
>     conf <- getConfig
>     withConn ("host=localhost dbname=" ++ dbName conf ++
>               " user=" ++ username conf ++
>               " password=" ++ password conf) (\conn -> do

get module paths:
get the module,parent pairs and create a map from module names to the
path from the root to that module

>       r0 <- selectRelationValues conn
>                 "select module_name, module_parent_name\n\
>                 \from modules order by module_order" []
>       let flookup = safeLookup "get module parents"
>           moduleMap = foldr addI [] r0
>               where addI i l = l ++ [(i!!0, i!!1)]
>           paths = map (\(c,p) -> (c,getPath c p)) moduleMap
>               where getPath c p = if p == "root"
>                                     then [c]
>                                     else getPath p (flookup p moduleMap)
>                                            ++ [c]

-- get database objects: add each one under the path to the module it is
-- contained in

>       r1 <- selectRelationValues conn
>                 "select module_name,object_type,object_name\n\
>                 \from module_objects\n\
>                 \natural inner join modules\n\
>                 \natural inner join object_orders\n\
>                 \order by module_order desc,object_order desc,object_name" []
>       let tree = foldr addNodeA [] r1
>              where addNodeA i tree =
>                        addNode tree $ flookup (i!!0) paths
>                                       ++ [i!!1,i!!2]
>       return tree))
>       (\e -> return [Node {rootLabel="Error: " ++ show e,
>                            subForest=[]}])

parse a file into a tree, we match headers (which start with one or
more = to create the branch structure, then add function defs or
object definitions as leafs depending on whether we are looking at an
lhs or sql file)

> parseFile filename = do
>   content <- readFile filename
>   let tr = case True of
>          _ | isSuffixOf ".lhs" filename ->
>                "^((={1,6} .*$)|\
>                \(> [A-Za-z].*=.*$))"
>            | isSuffixOf ".sql" filename ->
>                "^((={1,6} .*$)|\
>                \(create (function|table|view|domain) [A-Za-z0-9_]*)|\
>                \(select create_var.*$)|\
>                \(copy .*))"
>            | otherwise -> "^={1,6} .*$"
>       matches = getAllTextMatches (content =~ tr) :: [String]
>   return $ addNodeAt [] [] matches

addNodeAt keeps track of the current path as we go through the
matches line by line, so when we get the next match, if it is a header
we can adjust the path appropriately, if is is a leaf the we know
which path to add it under

>   where addNodeAt tree currentPath (l:ls) =
>             if isPrefixOf "=" l
>               then
>                 let h = length (takeWhile (=='=') l)
>                     p' = (if h > length currentPath
>                            then currentPath
>                            else take (h - 1) currentPath)
>                          ++ [l]
>                 in addNodeAt (addNode tree p') p' ls
>               else
>                 addNodeAt (addNode tree (currentPath ++ [l])) currentPath ls
>         addNodeAt tree currentPath [] = tree
