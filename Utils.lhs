
Copyright 2009 Jake Wheat

A bunch of utility functions, basic functional stuff, monad stuff,
file system stuff. Probably they are all in the standard library but I
didn't find them.

> module Utils (applyMany,
>               findAllFiles,
>               time,
>               timeName,
>               updateLookup,
>               hasKey,
>               safeLookup,
>               safeMLookup,
>               lk,
>               messageIfError,
>               dropItemN,
>               for,
>               uncurry3,
>               uncurry5,
>               toLower,
>               whenA,
>               whenA1,
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
> import Data.Maybe
> --import qualified Data.ByteString.Lazy as B
> --import qualified Data.ByteString as Bs
> import qualified Data.Map as M

> applyMany :: [(a -> b)] -> a -> [b]
> applyMany fns val =
>   reverse $ applyMany' fns val []
>   where
>     applyMany' [] _ r = r
>     applyMany' (f:fs) v r = applyMany' fs v (f v : r)

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

> hasKey :: (Eq k) => k -> [(k, v)] -> Bool
> hasKey k = any (\(k',_) ->  k == k')

lookup wrappers that throw a supplied error message to help with
tracking down problems

> safeLookup :: (Show a, Eq a) => String -> a -> [(a, b)] -> b
> safeLookup errMsg key lkp =
>     case lookup key lkp of
>       Just x -> x
>       Nothing -> error $ errMsg ++ " missing key: " ++ show key

> safeMLookup :: (Show k, Ord k) => String -> k -> M.Map k a -> a
> safeMLookup errMsg key lkp =
>     case M.lookup key lkp of
>       Just x -> x
>       Nothing -> error $ errMsg ++ " missing key: " ++ show key

> lk :: (Ord k) =>
>       k -> M.Map k [Char] -> [Char]
> lk k = fromMaybe "" . M.lookup k


> time :: IO c -> IO c
> time =
>   bracket getClockTime
>           (\st -> do
>              et <- getClockTime
>              let tdiff = diffClockTimes et st
>              putStrLn $ "time taken: " ++ timeDiffToString tdiff)
>           . const

> diffTimes :: ClockTime -> ClockTime -> Integer
> diffTimes (TOD sts stps) (TOD ets etps) = (ets * 1000 + div109 etps) -
>                                           (sts * 1000 + div109 stps)
>                                           where
>                                             div109 a = a `div`
>                                                ((10::Integer) ^ (9::Integer))


> timeName :: String -> IO c -> IO c
> timeName name =
>   bracket (do
>             x <- getClockTime
>             return x)
>           (\st -> do
>              et <- getClockTime
>              putStrLn $ name ++ ": " ++ show (diffTimes st et))
>           . const


> for :: [a] -> (a -> b) -> [b]
> for = flip map

> uncurry3 :: (t -> t1 -> t2 -> t3) -> (t, t1, t2) -> t3
> uncurry3 a (b,c,d) = a b c d

> uncurry5 :: (t -> t1 -> t2 -> t3 -> t4 -> t5)
>             -> (t, t1, t2, t3, t4)
>             -> t5
> uncurry5 a (b,c,d,e,f) = a b c d e f

> toLower :: String -> String
> toLower = map DC.toLower


> trim      :: String -> String
> trim      = f . f
>    where f = reverse . dropWhile DC.isSpace


run an external program, return the stdout, stderr and the exit
code. This doesn't work right for programs which produce a lot of
output.

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

> deleteIfExists :: FilePath -> IO ()
> deleteIfExists fn =
>    whenA (doesFileExist fn)
>          (removeFile fn)

> dropItemN :: [a] -> Int -> [a]
> dropItemN [] _ = []
> dropItemN (x:xs) i = if i == 0
>                        then xs
>                        else x: dropItemN xs (i - 1)

This doesn't work for some reason - doesn't print the message if there
is an error:

> messageIfError :: String -> IO a -> IO a
> messageIfError message =
>     bracketOnError (return())
>                    (const $ putStrLn message)
>                    . const

helpers for putting an io action in the condition clause of a when

> whenA1 :: IO a -> (a -> Bool) -> IO () -> IO ()
> whenA1 feed cond f = (cond `liftM` feed) >>= flip when f

> whenA :: IO Bool -> IO () -> IO ()
> whenA cond f = cond >>= flip when f
