
This file is the datatypes and drawing routine for defining and drawing a board

> module BoardSurface
>     (drawBoard
>     ,DrawingInstruction(..)
>     ) where

> import Graphics.Rendering.Cairo

> import Control.Monad
> import Data.Maybe

> import Resources

> data DrawingInstruction = DI Int Int
>                         | Spr Int Int Sprite
>                         | Beam Int Int Int Int
>                         | NullDI
>                         | SquishedSpr Int Int Double Sprite

> drawBoard :: SpriteMap -> [DrawingInstruction] -> Int -> Int -> Render ()
> drawBoard sm dis w' h' = do
>   let w = fi w'
>       h = fi h'

>   -- clear screen
>   setSourceRGB 0 0 0
>   paint
>
>   let px1 = 12 / (w + h) -- approximate avg for x and y

>   -- draw grid
>   setSourceRGB 0.5 0.5 0.5
>   setLineWidth px1
>   scale (w / 15) (h / 9)
>   forM_ [1..14] $ \x -> do
>     moveTo x 0
>     lineTo x h
>   forM_ [1..8] $ \y -> do
>     moveTo 0 y
>     lineTo w y
>   stroke

>   --draw dis
>   setSourceRGB 1 1 1
>   setLineWidth (px1 * 3)
>   forM_ dis $ \di ->
>     case di of
>       DI x y -> do
>                 moveTo (fi x + 1) (fi y + 0.5)
>                 arc (fi x + 0.5) (fi y + 0.5) 0.5 0 6.283
>                 stroke
>       Spr x y s -> do
>         let sp = fromMaybe (error $ "sprite not found: " ++ s) $ lookup s sm
>         drawSurface sp (fi x) (fi y) 1 1
>       Beam x0 y0 x1 y1 -> do
>         setSourceRGB 1 1 1
>         setLineWidth $ px1 * 15
>         moveTo (fi x0 + 0.5) (fi y0 + 0.5)
>         lineTo (fi x1 + 0.5) (fi y1 + 0.5)
>         stroke
>       SquishedSpr x y p s -> do
>         let sp = fromMaybe (error $ "sprite not found: " ++ s) $ lookup s sm
>         drawSurface sp (fi x) (fi y) 1 p
>       NullDI -> return ()
>   where
>     fi = fromIntegral

> drawSurface :: Surface -> Double -> Double -> Double -> Double -> Render ()
> drawSurface s x y w h = do
>   save
>   translate x y
>   sw <- imageSurfaceGetWidth s
>   sh <- imageSurfaceGetHeight s
>   scale (w / fi sw) (h / fi sh)
>   setSourceSurface s 0 0
>   paint
>   restore
>   where
>     fi = fromIntegral
