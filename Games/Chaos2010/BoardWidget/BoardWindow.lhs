
> module Games.Chaos2010.BoardWidget.BoardWindow (startBoardWindow) where

> import Graphics.UI.SDL as SDL hiding (flip, Event)
> import qualified Graphics.UI.SDL as SDL
> import Graphics.UI.SDL.Rotozoomer
> import qualified Graphics.UI.SDL.TTF.General as TTFGeneral
> import Graphics.UI.SDL.TTF.Management
> import Graphics.UI.SDL.TTF.Types
> import Graphics.UI.SDL.TTF.Render
> import qualified Graphics.UI.SDL.Events as E
> import qualified Graphics.UI.SDL.Image as Img

> import Control.Concurrent.Chan.Strict
> import Control.Monad
> import Data.Maybe
> import System.FilePath
> import System.Directory
> import System.FilePath.Find

> import qualified Games.Chaos2010.UI.UITypes as U

> startBoardWindow :: String -> Int -> Int -> Int -> Int -> Chan U.Event -> IO ()
> startBoardWindow title x y w h evChan = do
>   SDL.init [InitEverything]
>   spr <- loadSprites
>   setVideoMode w h 32 []
>   enableKeyRepeat 70 70
>   runHandler spr
>   where
>     runHandler spr = do
>       screen <- getVideoSurface
>       renderGrid spr tmp screen
>       SDL.flip screen
>       e <- waitEvent
>       when (e /= Quit) $
>           let w e = do
>                 putStrLn $ "sdl: " ++ show e
>                 writeChan evChan e
>           in case e of
>                           KeyDown (Keysym SDLK_UP _ _ ) -> w $ U.Key "up"
>                           KeyDown (Keysym SDLK_DOWN _ _ ) -> w $ U.Key "down"
>                           KeyDown (Keysym SDLK_RIGHT _ _ ) -> w $ U.Key "right"
>                           KeyDown (Keysym SDLK_LEFT _ _ ) -> w $ U.Key "left"
>                           KeyDown (Keysym SDLK_HOME _ _ ) -> w $ U.Key "home"
>                           KeyDown (Keysym SDLK_END _ _ ) -> w $ U.Key "end"
>                           KeyDown (Keysym x _ _) ->  putStrLn $ show x
>                           _ -> return ()
>       runHandler spr

> tmp = U.SpriteGrid 15 10 [(0,0,"wizard0")
>                        ,(7,0,"wizard1")
>                        ,(14,0,"wizard2")
>                        ,(0,4,"wizard3")
>                        ,(14,4,"wizard4")
>                        ,(0,9,"wizard5")
>                        ,(7,9,"wizard6")
>                        ,(14,9,"wizard7")]

> sdlGtkKeyMap :: [(SDLKey, String)]
> sdlGtkKeyMap = [(SDLK_UP, "up")]


> renderGrid :: SprLookup -> U.SpriteGrid -> Surface -> IO()
> renderGrid sprs (U.SpriteGrid w h is) surf = do
>   let swidth = surfaceGetWidth surf
>   let sheight = surfaceGetHeight surf
>   let sg1 = drawIns swidth sheight w h is
>   fillRect surf Nothing (Pixel 0)
>   mapM_ renderI sg1
>   where
>     renderI :: DrawingInstruction -> IO ()
>     renderI (Sprite x y w h s) = let image = fromMaybe unknownSprite $ lookup s sprs
>                                  in blitZoom image x y w h surf
>     --renderI (VLine x) = fillRect surf (Just $ Rect (dtoi x) 0 1 sheight) (Pixel 255) >> return ()
>     --renderI (HLine y) = fillRect surf (Just $ Rect 0 (dtoi y) swidth 1) (Pixel 255) >> return ()
>     --renderI (Text t) = do
>     --  tr <- renderTextSolid font t $ Color 255 255 255
>     --  blitSurface tr Nothing surf $ Just $ Rect 0 0 0 0
>     --  return ()
>     unknownSprite = fromJust $ lookup "unknown.0" sprs
>     drawIns :: Int -> Int -> Int -> Int -> [(Int,Int,String)] -> [DrawingInstruction]
>     drawIns tw th sw sh sp =
>         let sx = itod tw / itod sw
>             sy = itod th / itod sh
>             di1 = map (\(x,y,s) -> Sprite (itod x) (itod y) 1 1 (s ++ ".0")) sp
>         in translate (Scale sx sy) di1


> data DrawingInstruction = Sprite Double Double Double Double String
>                         -- | VLine Double
>                         -- | HLine Double
>                         -- | Text String
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
>   blitSurface s1 Nothing tgt $ Just $ Rect (dtoi x) (dtoi y) 0 0
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

> type SprLookup = [(String,Surface)]

> loadSprites :: IO SprLookup
> loadSprites = do
>   pngNames <- find always (extension ==? ".png") "data/sprites"
>   let spriteNames = map (dropExtension . snd . splitFileName) pngNames
>   let sprs = filter (\l -> takeExtension l == ".png") pngNames
>   ss <- mapM (Img.load) sprs
>   return $ zip spriteNames ss
