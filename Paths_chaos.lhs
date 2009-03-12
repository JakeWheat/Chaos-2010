> {-# OPTIONS  -cpp #-}

> module Paths_chaos where

#if !defined(CABAL)

> getDataFileName :: FilePath -> IO FilePath
> getDataFileName f = putStrLn "mine" >> return f

#endif
