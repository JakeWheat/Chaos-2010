
this file loads the pngs from the disk into surfaces arrange into
frames for each animated sprite

> {-# LANGUAGE TupleSections #-}
> module Resources
>     (loadPngs
>     ,AnimatedSprite(..)
>     ,Sprite
>     ,SpriteMap
>     ,AnimatedSpriteMap
>     ) where

> import Graphics.Rendering.Cairo

> import System.FilePath
> import System.FilePath.Find

> import Data.List hiding (partition,find)
> import Data.Char
> import Data.Ord

> import Control.Monad

> import Utils

> data AnimatedSprite = AnimatedSprite String [Sprite] Int -- animated sprite name, static sprite frame names, animation speed
>                       deriving Show

> type Sprite = String

> type SpriteMap = [(String,Surface)]
> type AnimatedSpriteMap = [(String,AnimatedSprite)]


> loadImage :: FilePath -> IO Surface
> loadImage fp =
>   withImageSurfaceFromPNG fp $ \is -> do
>     w <- imageSurfaceGetWidth is
>     h <- imageSurfaceGetHeight is
>     s <- createSimilarSurface is ContentColorAlpha w h
>     renderWith s $ do
>        setSourceSurface is 0 0
>        paint
>     return s


> loadPngs :: FilePath -> IO (AnimatedSpriteMap,SpriteMap) --[(String,[Surface])]
> loadPngs fld = do
>   -- get a list of files
>   files <- find (return True) (return True) fld
>   let files' = flip filter files $ \fn -> map toLower (takeExtension fn) == ".png"
>       sprNames :: [(String,FilePath)]
>       sprNames = flip map files' $ \f -> (takeFileName $ dropExtension f, f)
>       -- create a list of base name, frame number, sprite name triples
>       nms :: [(String,Int,String)]
>       nms = flip map (map fst sprNames)
>               $ \sn -> let (n,f) = break (=='.') sn
>                        in (n,read $ tail f, sn)
>       -- group by basename
>       sfs :: [(String,[(String,Int,String)])]
>       sfs = partition (\(n,_,_) -> n) nms
>       -- order by frame number
>       orderFrames :: (String, [(String,Int,String)])
>                   -> (String, [(String,Int,String)])
>       orderFrames (n,fs) = (n,sortBy (comparing (\(_,f,_) -> f)) fs)
>       sfso = map orderFrames sfs
>       amn = flip map sfso $ \(n,sns) -> (n, flip map sns $ \(_,_,sn) -> sn)
>       am = flip map amn $ \(n,sns) ->(n, AnimatedSprite n sns 10)
>   sm <- forM sprNames $ \(n,fn) -> fmap (n,) $ loadImage fn
>   --mapM_ (putStrLn . show) am
>   return (am,sm)
