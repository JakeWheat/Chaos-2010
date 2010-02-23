
> {-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
> module Games.Chaos2010.ConcreteUI.SDL.BoardWindow (startBoardWindow) where

> import Graphics.UI.SDL as SDL hiding (flip, Event)
> import qualified Graphics.UI.SDL as SDL
> import Graphics.UI.SDL.Rotozoomer
> --import qualified Graphics.UI.SDL.TTF.General as TTFGeneral
> --import Graphics.UI.SDL.TTF.Management
> --import Graphics.UI.SDL.TTF.Types
> --import Graphics.UI.SDL.TTF.Render
> --import qualified Graphics.UI.SDL.Events as E
> import qualified Graphics.UI.SDL.Image as Img

> import Control.Concurrent.Chan.Strict
> import Control.Concurrent(threadDelay)
> --import Control.Monad
> import Data.Maybe
> import System.FilePath
> --import System.Directory
> import System.FilePath.Find
> import Data.IORef

> import qualified Games.Chaos2010.UI.UITypes as U

> startBoardWindow :: String -> Int -> Int -> Int -> Int
>                  -> Chan U.Event -> Chan (String,U.WindowUpdate) -> IO ()
> startBoardWindow title x y w h evChan upChan = do
>   SDL.init [InitEverything]
>   spr <- loadSprites
>   _ <- setVideoMode w h 32 []
>   _ <- enableKeyRepeat 70 70
>   (sg::IORef U.SpriteGrid) <- newIORef $ U.SpriteGrid 15 10 []
>   startTime <- getTicks
>   runHandler startTime sg spr
>   where
>     runHandler startTime sg spr = do
>       --putStrLn "runhandler"
>       screen <- getVideoSurface
>       sg1 <- readNewGrid sg
>       currentTime <- getTicks
>       let elapsedTime = fromIntegral (currentTime - startTime)
>       renderGrid elapsedTime spr sg1 screen
>       SDL.flip screen
>       processEvents
>       --delay 1000
>       threadDelay $ 1000 * 100
>       runHandler startTime sg spr
>     processEvents = do
>       e <- pollEvent
>       --putStrLn $ "got " ++ show e
>       case e of
>              KeyDown (Keysym k _ _) | Just a <- lookup k sdlGtkKeyMap ->  writeChan evChan $ U.Key a
>                                     | otherwise -> putStrLn $ "ignored: " ++ show k
>              NoEvent -> return ()
>              _ -> processEvents
>     readNewGrid :: IORef U.SpriteGrid -> IO U.SpriteGrid
>     readNewGrid sg = do
>       mt <- isEmptyChan upChan
>       if mt
>         then readIORef sg
>         else do
>           u <- readChan upChan
>           case u of
>             (_,U.WUSpriteGrid r) -> putStrLn "updating board" >> writeIORef sg r >> return r
>             _ -> readIORef sg

> sdlGtkKeyMap :: [(SDLKey, String)]
> sdlGtkKeyMap = [(SDLK_UP, "up")
>                ,(SDLK_DOWN, "down")
>                ,(SDLK_LEFT, "left")
>                ,(SDLK_RIGHT, "right")]

> renderGrid :: Int -> SprLookup -> U.SpriteGrid -> Surface -> IO()
> renderGrid et sprs (U.SpriteGrid ww wh is) surf = do
>   let swidth = surfaceGetWidth surf
>   let sheight = surfaceGetHeight surf
>   let sg1 = drawIns swidth sheight ww wh is
>   _ <- fillRect surf Nothing (Pixel 0)
>   mapM_ renderI sg1
>   where
>     renderI :: DrawingInstruction -> IO ()
>     renderI (Sprite x y w h s) = let images = fromMaybe unknownSprite $ lookup s sprs
>                                      fr = (et `div` 250) `mod` length images
>                                      im = images !! fr
>                                  in blitZoom im x y w h surf
>     --renderI (VLine x) = fillRect surf (Just $ Rect (dtoi x) 0 1 sheight) (Pixel 255) >> return ()
>     --renderI (HLine y) = fillRect surf (Just $ Rect 0 (dtoi y) swidth 1) (Pixel 255) >> return ()
>     --renderI (Text t) = do
>     --  tr <- renderTextSolid font t $ Color 255 255 255
>     --  blitSurface tr Nothing surf $ Just $ Rect 0 0 0 0
>     --  return ()
>     unknownSprite = fromJust $ lookup "unknown" sprs
>     drawIns :: Int -> Int -> Int -> Int -> [(Int,Int,String)] -> [DrawingInstruction]
>     drawIns tw th sw sh sp =
>         let sx = itod tw / itod sw
>             sy = itod th / itod sh
>             di1 = map (\(x,y,s) -> Sprite (itod x) (itod y) 1 1 s) sp
>         in translate (Scale sx sy) di1


> data DrawingInstruction = Sprite Double Double Double Double String
>                         --   | VLine Double
>                         --   | HLine Double
>                         --   | Text String
>                           deriving Show

> data Translation = Move Double Double
>                  | Scale Double Double

> dtoi :: Double -> Int
> dtoi = truncate

> itod :: Int -> Double
> itod = fromInteger . toInteger

> blitZoom :: Surface -> Double -> Double -> Double -> Double -> Surface -> IO ()
> blitZoom src x y width height tgt = do
>   s1 <- zoom src (width / fromIntegral (surfaceGetWidth src))
>                  (height / fromIntegral (surfaceGetHeight src)) True
>   _ <- blitSurface s1 Nothing tgt $ Just $ Rect (dtoi x) (dtoi y) 0 0
>   return ()

> translate :: Translation -> [DrawingInstruction] -> [DrawingInstruction]
> translate t ds =
>   map tn ds
>   where
>     tn (Sprite x y w h s) = Sprite (tx x) (ty y) (sx w) (sy h) s
>     --tn (VLine x) = VLine (tx x)
>     --tn (HLine y) = HLine (ty y)
>     --tn (Text s) = Text s
>     tx x = case t of
>                   Move dx _ -> x + dx
>                   Scale dx _ -> x * dx
>     ty y = case t of
>                   Move _ dy -> y + dy
>                   Scale _ dy -> y * dy
>     sx x = case t of
>              Move _ _ -> x
>              Scale dx _ -> x * dx
>     sy y = case t of
>              Move _ _ -> y
>              Scale _ dy -> y * dy

> type SprLookup = [(String,[Surface])]

> loadSprites :: IO SprLookup
> loadSprites = do
>   pngNames <- find always (extension ==? ".png") "data/sprites"
>   let spriteNames = map (dropExtension . snd . splitFileName) pngNames
>   let sprs = filter (\l -> takeExtension l == ".png") pngNames
>   ss <- mapM (Img.load) sprs
>   let spf = zip (map getSpriteId spriteNames) ss
>       fld :: ((String,Int), Surface) -> [(String,[Surface])] -> [(String,[Surface])]
>       fld = (\((s,f),p) l -> insertWith (++) s [p] l)
>   return $ foldr fld [] spf


> getSpriteId :: String -> (String,Int)
> getSpriteId s =
>     let fn = snd $ splitFileName s
>         sn = dropExtension fn
>         f = takeExtension sn
>     in (sn,read (tail f))
