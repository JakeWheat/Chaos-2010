#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

> module BoardWidget (boardWidgetNew) where

> import Graphics.UI.Gtk
> import Graphics.Rendering.Cairo
>
> import Data.List
> import qualified Data.Map as M
> import qualified Data.Char as DC
> import Control.Monad
> import Data.Maybe
> import Data.IORef
> import System.Time

> import ChaosDB
> import Utils
> import qualified Logging
> import SoundLib
> import ChaosTypes

================================================================================

= Board Widget

Use cairo, draw sprites which are held in png files. There are sprites
with multiple frames for each piece, and also sprites to represent
cursors and square highlights.

The sprites are loaded in the loadSprites function and made available
as cairo surfaces.

Each sprite has a set of animation frames.  The two animation styles
are:

* forward: start with first frame then second, third, etc. till end
  then loop to first frame again,

* forward_backward: as forward but instead of looping to first go to
  second last, third last, etc. until second frame then start loop
  again at first frame. This means that first and last frames are only
  played once in each loop. This isn't done yet

Each sprite starts animating on the first frame when it is created:
e.g. two different goblins won't necessarily be at the same frame,
makes it look a bit better.

The frames for a sprite change at different speeds according to the
sprite.

TODO: visual and sound effects

> boardWidgetNew :: Connection -> SoundPlayer -> ColourList ->
>                   SpriteMap -> IO (Frame, IO())
> boardWidgetNew conn player _ spriteMap = do
>     frame <- frameNew
>     canvas <- drawingAreaNew
>     containerAdd frame canvas

Not really sure about the following code, robbed it from a mixture of
places, just wanted to get something working for now.

The widget needs to be redrawn quite a lot from expose and resize and
events like that, but we only need to access the database when the
board has changed. So - read it in the ctor and then in the refresh
method, and only read this cached data when handling an expose event

>     bd <- readBoardSprites
>     boardData <- newIORef bd
>     efc' <- selectTuples conn "select * from board_effects" []
>     effectsRef <- newIORef efc'
>     soundsRef <- newIORef ([]::[(Int,String)])

use getFrames to tell us how many frames have passed since the app was
started, this is used to determine which frame of each sprite to show

>     startTime' <- getClockTime

hook things up the the expose event

>     onExpose canvas
>              (\_ -> lg "boardWidgetNew.onExpose" "" $ do
>                        (w,h) <- widgetGetSize canvas
>                        drawin <- widgetGetDrawWindow canvas
>                        renderWithDrawable drawin
>                          (myDraw (getFrames' startTime') boardData soundsRef effectsRef
>                             (fromIntegral w) (fromIntegral h))

The following line use to read
                         return (eventSent x))
but that doesn't compile anymore, so just bodged it.

>                        return True)
>
>     let redraw = do
>           win <- widgetGetDrawWindow canvas
>           reg <- drawableGetClipRegion win
>           drawWindowInvalidateRegion win reg True
>           drawWindowProcessUpdates win True

>     let refresh startTime = lg "boardWidgetNew.refresh" "" $ do
>           --update the frame positions
>           f <- getFrames' startTime'
>           dbAction conn "update_missing_startframes" [show f]

Effects
The effects are loaded into the board_effects table.

There are two sorts of visual effects: beam and square, when we have
both, we want to do the beam before the square effect. Each effect can
have a sound effect associated with it. Sometimes we have an
additional sound effect that plays after the square e.g. a ranged
attack has a beam and a sound, then a square and a sound to indicate
the creature being attacked, then if the creature dies it ha a further
sound (possibly another effect?).

Whilst this happens we don't want to update the sprite list.

When the board widget refresh is called, if there are entries in the
effects table, save these tuples and set their start frames, clear the
effects table and skip updating the sprite list.

When the board widget refresh is called and the saved effects are not
empty, also skip refreshing the board sprites.

When the effects have finished playing (usually ~0.5 secs), clear the
local copy of the effects and refresh the sprites.

This allows the effect to play out before the sprites are changed

>           --check for effects
>           efc <- selectTuples conn "select * from board_effects" []
>           if length efc > 0
>             then do
>               runSql conn "update running_effects_table\n\
>                           \set running_effects = true" []
>               --mapM_ (\t -> putStrLn $ lk "type" t ++ lk "sound" t) efc
>               cf <- getFrames' startTime
>               let addStartFrame t = let sf = if lk "type" t == "beam"
>                                                then cf
>                                                else cf + 6
>                                     in M.insert "start_frame" (show sf) t
>                   removeSounds t = if lk "type" t == "sound"
>                                      then Nothing
>                                      else Just t
>               writeIORef effectsRef $ catMaybes $ map removeSounds $ map addStartFrame efc
>               let soundEffects = map (\t -> if lk "type" t /= "square"
>                                               then (cf, lk "sound" t)
>                                               else (cf + 12, lk "sound" t)) efc
>               --putStrLn "soundeffects:"
>               --print soundEffects
>               writeIORef soundsRef soundEffects
>               --play sound effects
>               --forM_ efc (\ef -> play player $ lk "sound" ef)
>               runSql conn "delete from board_effects" []
>             else do
>               efcE <- readIORef effectsRef
>               if (length efcE > 0)
>                 then
>                   return ()
>                   --putStrLn "continue effects"
>                 else do
>                   --putStrLn "read db for board"
>                   re <- selectValueIf conn "select running_effects from running_effects_table;" []
>                   when ((isJust re) && (read $ fromJust re)) $
>                        runSql conn "update running_effects_table\n\
>                                    \set running_effects = false" []
>                   bd1 <- readBoardSprites
>                   writeIORef boardData bd1
>           redraw

update the board sprites 10 times a second to animate them

>     flip timeoutAdd 100 $ do
>       widgetQueueDrawArea canvas 0 0 2000 2000
>       return True

>     return (frame, refresh startTime')
>     where
>         readBoardSprites =
>           selectTuplesC conn "select * from board_sprites" [] $
>                         \bs -> (read $ lk "x" bs::Int,
>                                 read $ lk "y" bs::Int,
>                                 lk "sprite" bs,
>                                 read $ lk "start_frame" bs::Int,
>                                 read $ lk "animation_speed" bs::Int)

>         getFrames' startTime = do
>           t <- getClockTime
>           let tdiff = diffClockTimes t startTime
>           --25 frames per second
>               ps = ((tdPicosec tdiff * 25::Integer) `div`
>                      ((10::Integer)^(12::Integer)))::Integer
>               f1 = fromIntegral (tdMin tdiff * 25 * 60) +
>                    fromIntegral (tdHour tdiff * 25 * 60 * 60) +
>                    fromIntegral (tdSec tdiff * 25) +  ps
>               b = fromInteger f1
>           return b::IO Int


>         myDraw getFrames boardData soundsRef effectsRef w h = do
>           --make the background black
>           setSourceRGB 0 0 0
>           paint
>

setup some helper functions:

>           let boardWidth = 15
>               boardHeight = 10

work out the size of each square in cairo co-ords

>               squareWidth = (w / boardWidth) ::Double
>               squareHeight = (h / boardHeight) ::Double

create toX and toY functions, you pass these the square
position and it returns the drawing co-ords of the top
left of that square

>               toX a = a * squareWidth
>               toY b = b * squareHeight


>               drawGrid = do
>                        setSourceRGB 0.2 0.2 0.2
>                        --draw vertical gridlines
>                        mapM_ (\x -> do
>                              moveTo (toX x) 0
>                              lineTo (toX x) (toY 10)) [1..14]
>                        setLineWidth 1
>                        stroke
>                        --draw horizontal gridlines
>                        mapM_ (\y -> do
>                              moveTo 0 (toY y)
>                              lineTo (toX 15) (toY y)) [1..9]
>                        setLineWidth 1
>                        stroke
>
>           drawGrid

--------------------------------------------------
draw sprites

assume sprites are 64x64

>           let sw = 64
>               sh = 64
>

get our scale factors so that the sprites are drawn at the same size
as the grid squares

>               scaleX = squareWidth / sw
>               scaleY = squareHeight / sh
>

use a scale transform on the cairo drawing surface to scale the
sprites. This seems a bit backwards since we have to generate new toX
and toY functions which take into account the changed scale factor.

>           scale scaleX scaleY
>           let toXS a = a * squareWidth / scaleX
>               toYS b = b * squareHeight / scaleY
>

create a helper function to draw a sprite at board position x,y
identifying the sprite by name, hiding all that tedious map lookup
stuff

>           cf <- liftIO getFrames
>           let drawAt x' y' sp sf as = do
>                 let x = (fromIntegral x')
>                     y = (fromIntegral y')
>                     p = safeMLookup "board widget draw" sp spriteMap
>                     (_,_,img) = p
>                     f = ((cf - sf) `div` as) `mod` length img
>                 setSourceSurface (img !! f) (toXS x) (toYS y)
>                 paint

Draw the board sprites from the saved board

>           bd2 <- liftIO $ readIORef boardData
>           mapM_ (uncurry5 drawAt) bd2


run any sounds which need to be run

>           snds <- liftIO $ readIORef soundsRef
>           when (length snds > 0) $ do
>             --liftIO $ putStrLn $ show cf
>             --liftIO $ mapM_ putStrLn $ map (\(a,b) -> show a ++ show b) snds
>             leftSnds <- mapM (\(t,s) -> if t <= cf
>                                         then do
>                                           --liftIO$ putStrLn $ "playing " ++ s ++ " at " ++ show t
>                                           liftIO $ play player s
>                                           return Nothing
>                                         else return $ Just (t,s)) snds
>             liftIO $ writeIORef soundsRef $ catMaybes leftSnds

draw the effects

>           efs <- liftIO $ readIORef effectsRef
>           when (length efs > 0) $ do
>             let drawEffectLine x1 y1 x2 y2 = do
>                                      setSourceRGB 0.7 0.7 0.7
>                                      moveTo (toXS $ x1 + 0.5) (toYS $ y1 + 0.5)
>                                      lineTo (toXS $ x2 + 0.5) (toYS $ y2 + 0.5)
>                                      setLineWidth 10
>                                      stroke

>             let drawEffectSquare x1 y1 = drawAt x1 y1 "effect_attack" 0 250

>             --liftIO $ putStrLn $ "got " ++ show (length efs) ++ "effects"
>             --remove expired effects
>             let efs1 = filter (\t -> (read (lk "start_frame" t)) + 6 >= cf) efs
>             --liftIO $ putStrLn $ show (length efs1) ++ "effects still good"
>             --liftIO $ putStrLn $ show cf ++ " current frame"
>             --mapM_ (\t -> liftIO $ putStrLn (lk "start_frame" t)) efs
>             forM_ efs1 $ \t -> do
>               let l f = lk f t
>               let esf = read $ l "start_frame"
>               when (esf <= cf) $ do
>                 --liftIO $ putStrLn "draw effect"
>                 let rd f = (read $ l f)::Double
>                 let ri f = (read $ l f)::Int
>                 if l "type" == "beam"
>                   then do
>                     drawEffectLine (rd "x1") (rd "y1") (rd "x2") (rd "y2")
>                   else do
>                     drawEffectSquare (ri "x1") (ri "y1")
>             --update the list of effects to remove the expired ones
>             liftIO $ writeIORef effectsRef efs1
>             -- if there are no more effects then refresh the sprites from the database
>             when (length efs1 == 0)$ do
>               bd1 <- liftIO readBoardSprites
>               liftIO $ writeIORef boardData bd1




> lg :: String -> String -> IO c -> IO c
> lg l m = Logging.pLog ("chaos.BoardWidget." ++ l) m
