> {-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, FlexibleContexts,TypeSynonymInstances #-}
> module Games.Chaos2010.Utils where
> import System.Time
> --import Database.HaskellDB
> import Control.Exception
> --import Database.HaskellDB.Database
> --import Test.HUnit


> insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
> insertWith ac k v m =
>     case lookup k m of
>       Nothing -> m ++ [(k,v)]
>       Just v' -> let nv = ac v' v
>                  in map (\p@(k1,_) -> if k1 == k
>                                       then (k1,nv)
>                                       else p) m

> safeLookup :: (Show a, Eq a) => String -> a -> [(a, b)] -> b
> safeLookup errMsg key lkp =
>     case lookup key lkp of
>       Just x -> x
>       Nothing -> error $ errMsg ++ " missing key: " ++ show key

 > whenA1 :: IO a -> (a -> Bool) -> IO () -> IO ()
 > whenA1 feed cond f = (cond `liftM` feed) >>= flip when f

 > dropItemN :: [a] -> Int -> [a]
 > dropItemN [] _ = []
 > dropItemN (x:xs) i = if i == 0
 >                        then xs
 >                        else x: dropItemN xs (i - 1)


> time :: IO c -> IO c
> time =
>   bracket getClockTime
>           (\st -> do
>              et <- getClockTime
>              let tdiff = diffClockTimes et st
>              putStrLn $ "time taken: " ++ timeDiffToString tdiff)
>           . const

