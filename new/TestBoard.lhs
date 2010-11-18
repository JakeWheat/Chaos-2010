
prototype to display sprites on screen using cairo and gtk.

stages:
draw some sprites in a window
draw some sprites from a data structure, update the window by sending in new data structure
add animation -> split frameless description from framified description
updates with animation
add effects
add timed sequence of boards
do client server with 1 thread doing updates to board, 1 thread to
draw backbuffers, and gtk thread


write demo of each full action effect + easy ability to play all or
specific subset

> {-# LANGUAGE TupleSections,ScopedTypeVariables #-}


> import Control.Monad.State

> import Data.IORef

> import Control.Concurrent.Chan
> import Control.Concurrent

> import Graphics.Rendering.Cairo

> import Resources
> import BoardSurface
> import Gui
> import Input

-------------------------------------------

game state


> makeBigDIs :: [(String,[Surface])] -> [DrawingInstruction]
> makeBigDIs sprs =
>   dis ++ sprsList
>   where
>     dis = [DI 7 4
>           ,DI 0 0
>           ,DI 0 8
>           ,DI 14 0
>           ,DI 14 8
>           ]
>     sprsList = concat $ flip evalState (map fst sprs)
>                $ forM [0..14] $ \x -> forM [0..8] $ \y -> do
>                    s' <- get
>                    case s' of
>                            (h:t) -> put t >> return (Spr x y h)
>                            [] -> return (Spr x y $ fst $ head sprs)

> main :: IO ()
> main = do
>   sprs <- loadPngs "data/sprites"
>   let dis = [DI 7 4
>             ,DI 0 0
>             ,DI 0 8
>             ,DI 14 0
>             ,DI 14 8
>             ,Spr 0 0 "cursor"
>             ] -- makeBigDIs sprs
>   br <- newIORef dis
>   _ <- forkIO $ updateBoard br
>   inc <- newChan
>   _ <- forkIO $ handleInput br inc
>   frameCount <- newIORef 0
>   runGui inc (drawToBackBuffer sprs br frameCount)

> handleInput :: IORef [DrawingInstruction] -> Chan Input -> IO ()
> handleInput br inc = do
>   k <- readChan inc
>   putStrLn $ "got key: " ++ show k
>   case k of
>     IKey "Right" -> move (1,0)
>     IKey "Left" -> move (-1,0)
>     IKey "Up" -> move (0,-1)
>     IKey "Down" -> move (0,1)
>     _ -> return ()
>   handleInput br inc
>   where
>     move (x,y) =
>       atomicModifyIORef br $ \b ->
>         (flip map b $ \p -> case p of
>                              Spr cx cy "cursor" ->
>                                let cx' = min 14 $ max 0 $ cx + x
>                                    cy' = min 8 $ max 0 $ cy + y
>                                in Spr cx' cy' "cursor"
>                              z -> z
>         ,())


------------------------------------------------

> updateBoard :: IORef [DrawingInstruction] -> IO ()
> updateBoard br =
>   updateBoard' 0
>   where
>     updateBoard' fc = do
>       let pos = fc `mod` 14
>       atomicModifyIORef br $ \b ->
>         let b' = filter notSword b
>         in (Spr pos 1 "wizard_magic_sword" : b', ())
>       threadDelay $ 1000 * 500
>       updateBoard' (fc + 1)
>     notSword (Spr _ _ "wizard_magic_sword") = False
>     notSword _ = True

> drawToBackBuffer :: [(String,[Surface])] -> IORef [DrawingInstruction] -> IORef Int -> IORef Surface -> IO ()
> drawToBackBuffer sprs br fcr ios = do
>       s <- readIORef ios
>       w <- imageSurfaceGetWidth s
>       h <- imageSurfaceGetHeight s
>       fc <- readIORef fcr
>       writeIORef fcr $ fc + 1
>       --putStrLn $ "draw " ++ show fc
>       withSimilarSurface s ContentColorAlpha w h $ \bs -> do
>         -- draw the new board
>         renderWith bs $ do
>           dis <- liftIO $ readIORef br
>           drawBoard sprs dis fc (fromIntegral w) (fromIntegral h)
>         -- copy the board to the backbuffer
>         s' <- readIORef ios -- bit dodgy - the surface may have been finished
>                             -- during drawing, so get it again here
>         renderWith s' $ do --probably also a bit dodgy
>           setSourceSurface bs 0 0
>           paint
