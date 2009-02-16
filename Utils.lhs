> module Utils (applyMany,
>               findAllFiles,
>               time,
>               updateLookup,
>               hasKey,
>               for,
>               uncurry3,
>               uncurry5,
>               toLower,
>               run,
>               deleteIfExists,
>               trim) where

> import System.Directory
> import Control.Monad
> import System.Time
> import qualified Data.Char as DC
> import System.Exit
> import System.Process
> import System.IO
> import Control.Exception
> import qualified Data.ByteString.Lazy as B
> import qualified Data.ByteString as Bs

> applyMany :: [(a -> b)] -> a -> [b]
> applyMany fns val =
>   reverse $ applyMany' fns val []
>   where
>     applyMany' [] val r = r
>     applyMany' (f:fs) val r = applyMany' fs val (f val : r)

> findAllFiles :: String -> IO [String]
> findAllFiles folder = do
>   entries <- getDirectoryContents folder
>   let entries' = map (\f -> folder ++ "/" ++ f)
>                  (filter (\l -> l /= "." && l /= "..") entries)
>   files <- filterM doesFileExist entries'
>   folders <- filterM doesDirectoryExist entries'
>   recurseFolders <- mapM findAllFiles folders
>   let recurseFolders' = concat recurseFolders
>   return $ files ++ recurseFolders'

> updateLookup :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
> updateLookup k v lkp =
>     (k,v):filter (\(k',_) -> k' /= k) lkp

> hasKey :: Eq k => k -> [(k,v)] -> Bool
> hasKey k lkp = any (\(k',v) ->  k == k') lkp

> time f = do
>   st <- getClockTime
>   f
>   et <- getClockTime
>   let tdiff = diffClockTimes et st
>   putStrLn $ "time taken: " ++ timeDiffToString tdiff

> for = flip map


> uncurry3 a (b,c,d) = a b c d
> uncurry5 a (b,c,d,e,f) = a b c d e f

> toLower :: String -> String
> toLower = map DC.toLower


> trim      :: String -> String
> trim      = f . f
>    where f = reverse . dropWhile DC.isSpace


> run :: String -> IO (String,String,Int)
> run s = do
>     (ih,oh,eh,pid) <- runInteractiveCommand s
>     so <- hGetContents oh
>     se <- hGetContents eh
>     hClose ih
>     ex <- waitForProcess pid
>     evaluate (so,se, en ex)
>     where
>       en ex = case ex of
>                 ExitFailure e -> e
>                 _             -> 0

> deleteIfExists fn = do
>    x <- doesFileExist fn
>    if x
>     then removeFile fn
>     else return ()
