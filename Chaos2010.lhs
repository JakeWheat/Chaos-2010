#! /usr/bin/env runghc

> {-# LANGUAGE FlexibleContexts,ScopedTypeVariables #-}

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HaskellDB
> --import Data.List

> --import Games.Chaos2010.Database.Pieces_mr

> import Games.Chaos2010.GtkUI.GtkGUI

> main :: IO ()
> main =
>    postgresqlConnect [("dbname", "chaos")] $ \db -> do
>    startGUI db
>   {-postgresqlConnect [("dbname", "chaos")] $ \db -> do
>   --res <- query db $ table pieces_mr
>   --mapM_ putStrLn $ map (\r -> show $ r # ptype) res
>   let --l :: (HasField Ptype a1 a) => (Query (Rel Pieces_mr), [a] -> [String])
>       m :: Wrap
>       m = mkQ (table pieces_mr) (map (\r -> show $ r # ptype))
>       -- l = doQ db (table pieces_mr) (map (\r -> show $ r # ptype))
>       n = mkQ (table pieces_mr) (map (\r -> show (r # ptype) ++ show (r # allegiance)))
>       x = [m,n]
>       --a = ((table pieces_mr), (map (\r -> show $ r # ptype)))
>   --l >>= mapM_ putStrLn
>   --m db >>= mapM_ putStrLn
>   --n db >>= mapM_ putStrLn
>       
>   (z::[[String]]) <- mapM (\y -> y db) x >>= \x -> return $! x
>   putStrLn $ intercalate "\n" $ map (intercalate "\n") z
>   --putStrLn $ (intercalate "\n" . intercalate "\n") z
>   return ()

> type Wrap = Database -> IO [String]

> doQ db tb r = do
>    res <- query db tb
>    return $ r res

> mkQ tb r = \db -> do
>   res <- query db tb
>   return $ r res -}

 > data Renderer = Renderer