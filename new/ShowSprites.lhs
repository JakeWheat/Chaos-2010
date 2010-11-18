
> import Graphics.Rendering.Cairo

> import Graphics.UI.Gtk hiding (get,eventKeyName)
> --import Graphics.Rendering.Cairo

 > import Graphics.Transform.Magick.Images
 > import Graphics.Transform.Magick.Types hiding (Rectangle(..), filename)

> import System.Posix.Process

> import System.Directory
> import System.Cmd

> --import Control.Exception
> --import System.FilePath

> import Control.Monad
> import Data.Maybe
> import System
> import Data.List

> import Resources

> main :: IO ()
> main = do
>   x <- getArgs
>   let args = intercalate " " x
>   (asm,sm) <- loadPngs "data/sprites"
>   let sps = flip map asm $ \(_, AnimatedSprite _ (s:_) _) -> s
>       spsSurf = flip map sps $ \s -> fromMaybe (error $ "sprite not found: " ++ s)
>                                        $ lookup s sm
>   spsSurf1 <- mapM (transformSurface args) spsSurf
>   _ <- initGUI
>   canvas <- drawingAreaNew
>   frame <- frameNew
>   window <- makeWindow "Chaos 2010" 1200 720
>   containerAdd window frame
>   containerAdd frame canvas
>   widgetShowAll window

>   _ <- onExpose canvas $ const $ do
>        drw <- widgetGetDrawWindow canvas
>        renderWithDrawable drw $ drawStuff spsSurf1 1200 700
>        return True

>   _ <- onDestroy window mainQuit
>   mainGUI

> drawStuff :: [Surface] -> Int -> Int -> Render ()
> drawStuff sprs w' h' = do
>   let w = fi w'
>       h = fi h'
>   let px1 = 12 / (w + h) -- approximate avg for x and y


>   setSourceRGB 0 0 0
>   paint
>
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
>   drawSprites 0 0 sprs

>   where
>     drawSprites x y (s:ss) = do
>       drawSurface s (fi x) (fi y) 1 1
>       let (x',y') = newPos x y
>       drawSprites x' y' ss
>     drawSprites _ _ [] = return ()
>     fi = fromIntegral
>     newPos x y =
>          case x of
>            14 -> (0, y + 1)
>            _ -> (x+1,y)



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


> makeWindow :: String -> Int -> Int -> IO Window
> makeWindow title width height = do
>   window <- windowNew
>   set window [windowTitle := title
>              ,windowDefaultWidth := width
>              ,windowDefaultHeight := height]
>   return window

> transformSurface :: String -> Surface -> IO Surface
> transformSurface args s =
>   withTempFile "." "png" $ \fn -> do
>   surfaceWriteToPNG s fn
>   withTempFile "." "png" $ \fno -> do
>     _ <- system $ "convert " ++ args ++ " " ++ fn ++ " " ++ fno
>     imageSurfaceCreateFromPNG fno


> withTempFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
> withTempFile tmp_dir extn action = do
>   x <- getProcessID
>   findTempName x
>   where
>     findTempName x = do
>          let filename = ("tmp" ++ show x) ++ "." ++ extn
>              path = tmp_dir ++ "/" ++ filename
>          b  <- doesFileExist path
>          if b
>            then findTempName (x+1)
>            else do
>              r <- action path
>              --removeFile path
>              return r

