Copyright 2009 Jake Wheat

= Board Widget

The board widget uses cairo to draw a board based around a grid
(15x10) with sprites in the squares. Each sprite is animated (at
different speeds), and basic effects are also provided to give cues
whn actions occur. Sound effects accompany these visual
effects. (These effects were going to be left till later but it's
impossible to follow what is going on in a game without them).

The way the board widget works is shaped by the following constraints:

* we need to update the cairo canvas several times a second to animate
  the sprites.

* reading a fresh copy of the board_sprites relvar is quite slow, so
  we want to avoid this unless it has changed

* because of the way the server code works, the client only sees an
  action has occured after the action has completed (from the action
  history). E.g if the action is a successful attack, the client sees
  the database before the attack is started, and then only sees the
  full results of the attack: the attacked piece has gone, and the
  attacking piece has been moved. But we want to overlay the attack
  effects on the previous board layout: with the attacked piece still
  there and the attacking piece in its starting position.

* the board has to be responsive, it is much more important that the
  board updates in a timely fashion than the rest of the interface,
  especially when the player is moving the cursor around (i.e. if a
  cursor move doesn't update the board for 0.2 seconds, this is a
  failure and will result in a crap user experience, but if the info
  widget doesn't update for 0.2 secs after the cursor has been moved,
  this isn't really bad).

* to keep the app responsive, we want to keep the database querying
  and update functions and draw the board to an offscreen surface
  outside of a gtk handler

The database design is as follows:

board_sprites_cache
board_effects
check effects after actions, hold off updating cache

sequence:
run key_pressed or ai_continue
fill effects tables after action has been run (from new history entries)
loop: set current effects
      draw effects and board_sprites (not update since before actions
      from key pressed/ ai continue)
      loop while there are effects
update the board_sprites cache
add timeout to run the next ai_continue if available

in between the key_pressed/ai_continue and adding the timeout at the
bottom, the client doesn't respond to player game actions except
cursor move (still responds to window changes, closing app, fiddling
with new game widget)


Animation:

The sprites are held in png files. One animated sprite might have
several pngs corresponding to the different animation frames of that
sprite. The frames of a sprite are only used for animation and not for
state, e.g. a dead monster sprite is a different sprite to the
monster, and not just one of the frames for that monster.

The sprites animate at different speeds (the speed is in the sprites
table, and is in ticks (25 per second)). Todo: different sprites have
different animation styles: looped and back and forth. E.g. for a
looped style sprite with 4 frames, the order shown is 1 2 3 4 1 2 3 4,
etc., for a back and forth one it is 1 2 3 4 3 2 1 2 3, etc.

Timing:

The timing uses ticks (25 ticks per second). Not really sure why,
should probably change to milliseconds or something. When the board
widget is created, it saves the current time, and then everything is
cued using the number of ticks since this saved time to the current
time (this is used by the sprite animation and the effects).



> module BoardWidget (boardWidgetNew) where

> import Graphics.UI.Gtk
> import Graphics.Rendering.Cairo
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad
> import Data.Maybe
> import Data.IORef
> import System.Time

 > import Control.Concurrent

> import ChaosDB
> import Utils
> import qualified Logging
> import SoundLib
> import ChaosTypes
> import ThreadingUtils

================================================================================

= data types

> data BoardSprite = BoardSprite {
>      bsx :: Int
>     ,bsy :: Int
>     ,bsSprite :: String
>     ,bsAllegiance :: String
>     ,bsColour :: String
>     ,bsStartTick :: Int
>     ,bsASpeed :: Int
>     ,bsSelected :: Bool}

> type SoundEffect = (String,String)
> type SquareEffect = (String,Int,Int)
> type BeamEffect = (String,Int,Int,Int,Int)
> type EffectsLine = ([BeamEffect]
>                    ,[SquareEffect]
>                    ,[SoundEffect])


================================================================================

= Ctor

> boardWidgetNew :: Connection -> SoundPlayer -> ColourList ->
>                   SpriteMap -> IO() -> IO (Frame, IO())

> boardWidgetNew conn player colours spriteMap queueAiUpdate = do
>   --setup the gtk widgets
>   frame <- frameNew
>   canvas <- drawingAreaNew
>   containerAdd frame canvas

>   dbAction conn "reset_current_effects" []

used to calculate how many ticks since the app was started, to time
the animations and effects:

>   startTicks <- getClockTime

create the initial draw surface and io ref

>   surf <- createImageSurface FormatARGB32 100 100
>   surfRef <- newIORef surf

>   fbr@(forkBR,_) <- forkAndQueueOneNew
>   let refresh = refreshA fbr conn colours player spriteMap
>                          canvas surfRef startTicks queueAiUpdate

hook the onexpose event up to drawing the saved surface

>   onExpose canvas $ \_ -> lg "boardWidgetNew.onExpose" "" $ do
>     --putStrLn "start expose"
>     drawin <- widgetGetDrawWindow canvas
>     renderWithDrawable drawin $ do
>       s' <- liftIO $ readIORef surfRef
>       setSourceSurface s' 0 0
>       paint
>     --todo: if the canvas has changed size, queue an immediate redraw
>     --putStrLn "end expose"
>     return True

queue the first board update

>   flip idleAdd priorityDefaultIdle $ do
>     forkBR refresh
>     return False

>   return (frame, return())

================================================================================

= refresh

The refresh function loads the board sprites and effects data from the
database and draws them to the cached surface

> refreshA :: Forker
>          -> Connection
>          -> ColourList
>          -> SoundPlayer
>          -> SpriteMap
>          -> DrawingArea
>          -> IORef Surface
>          -> ClockTime
>          -> IO()
>          -> IO ()
> refreshA fbr@(forkBR, doneBR) conn colours player spriteMap canvas
>          surfRef startTicks queueAiUpdate =
>   lg "boardWidgetNew.refreshA" "" $ do
>   --putStrLn "startrefresha"

== read database

>   ticks <- getTicks startTicks
>   effectsLine <- readCurrentEffects conn ticks
>   bd <- readBoardSprites conn
>   (surf,w,h) <- getSurface

== redraw surface

>   let boardWidth = 15
>       boardHeight = 10
>       squareWidth = (fromIntegral w / boardWidth)
>       squareHeight = (fromIntegral h / boardHeight)

>   renderWith surf $ do
>     setSourceRGB 0 0 0
>     paint
>     drawGrid squareWidth squareHeight

assume sprites are 64x64
get our scale factors so that the sprites are drawn at the same size
as the grid squares

>     let sw = 64
>         sh = 64
>         scaleX = squareWidth / sw
>         scaleY = squareHeight / sh
>

use a scale transform on the cairo drawing surface to scale the
sprites. This seems a bit backwards since we have to generate new toX
and toY functions which take into account the changed scale factor.

>     scale scaleX scaleY
>     let toXS a = a * squareWidth / scaleX
>         toYS b = b * squareHeight / scaleY
>     drawSprites ticks colours spriteMap toXS toYS bd
>     runEffects spriteMap player effectsLine ticks toXS toYS

== queue draw

>   flip idleAdd priorityDefaultIdle $ do
>     widgetQueueDrawArea canvas 0 0 2000 2000
>     flip timeoutAdd 100 $ do
>       forkBR $ refreshA fbr conn colours player spriteMap
>                         canvas surfRef startTicks queueAiUpdate
>       return False
>     doneBR
>     when (emptyEl effectsLine) queueAiUpdate
>     return False
>   return ()
>     where
>       getSurface = do
>                    surf' <- readIORef surfRef
>                    --see if the canvas size is changed
>                    (w,h) <- widgetGetSize canvas
>                    surw <- imageSurfaceGetWidth surf'
>                    surh <- imageSurfaceGetHeight surf'
>                    surf <- if ((w,h) == (surw,surh))
>                              then return surf'
>                              else do
>                                   surfaceFinish surf'
>                                   s <- createImageSurface FormatARGB32 w h
>                                   writeIORef surfRef s
>                                   return s
>                    return (surf,w,h)
>       emptyEl (a,b,c) = null a && null b && null c

== drawing helpers

Draw the board sprites from the saved board

> drawSprites :: Int
>             -> ColourList
>             -> SpriteMap
>             -> (Double -> Double)
>             -> (Double -> Double)
>             -> (String,[BoardSprite])
>             -> Render ()
> drawSprites cf colours spriteMap toXS toYS (currentWiz, bd2) = do
>   mapM_ (drawAllegiance colours currentWiz toXS toYS) $
>         filter (\bs -> not $ (bsAllegiance bs) `elem` ["", "dead"]) bd2
>   mapM_ (drawAt spriteMap toXS toYS cf) bd2

process the effects queue, remove any effects which have finished,
play any new sounds, draw any current visual effects

> runEffects :: SpriteMap
>            -> SoundPlayer
>            -> EffectsLine
>            -> Int
>            -> (Double -> Double)
>            -> (Double -> Double)
>            -> Render ()
> runEffects spriteMap player (beamEffects
>                             ,squareEffects
>                             ,soundEffects) ticks toXS toYS = do
>   liftIO $ mapM_ (play player . snd) soundEffects
>   mapM_ drawBeamEffect beamEffects
>   mapM_ drawSquareEffect squareEffects
>     where
>       drawBeamEffect (_,x1,y1,x2,y2) = do

-- >         liftIO $ putStrLn $ "beam: " ++ show x1 ++ " "
-- >                      ++ show y1 ++ " "
-- >                             ++ show x2 ++ " "
-- >                                    ++ show y2 ++ " "

>         let fi = fromIntegral
>         setSourceRGB 0.7 0.7 0.7
>         moveTo (toXS $ fi x1 + 0.5) (toYS $ fi y1 + 0.5)
>         lineTo (toXS $ fi x2 + 0.5) (toYS $ fi y2 + 0.5)
>         setLineWidth 10
>         stroke

>       drawSquareEffect(_,x1,y1) =
>         drawAt spriteMap toXS toYS ticks
>           (BoardSprite x1 y1 "effect_attack" "" "" 0 250 False)

helper function to draw a sprite at board position x,y identifying the
sprite by name, hiding all that tedious map lookup stuff

> drawAt :: SpriteMap
>        -> (Double -> Double)
>        -> (Double -> Double)
>        -> Int
>        -> BoardSprite
>        -> Render ()
> drawAt spriteMap toXS toYS cf bs = do -- sprite start tick, animation speed
>   let p = safeMLookup "board widget draw" (bsSprite bs) spriteMap
>       (_,_,img) = p
>       f = (cf - (bsStartTick bs)) `div` (bsASpeed bs) `mod` length img
>   setSourceSurface (img !! f)
>            (toXS $ fromIntegral (bsx bs))
>            (toYS $ fromIntegral (bsy bs))
>   paint

> drawAllegiance :: ColourList
>                -> String
>                -> (Double -> Double)
>                -> (Double -> Double)
>                -> BoardSprite
>                -> Render ()
> drawAllegiance colours currentWizard toXS toYS bs = do
>   let fi = fromIntegral
>   let (Color r g b) = safeLookup "get piece allegiance colour"
>                         (bsColour bs) colours
>   setSourceRGB (fi r) (fi g) (fi b)
>   arc (toXS $ 0.5 + (fromIntegral $ bsx bs))
>       (toYS $ 0.5 + (fromIntegral $ bsy bs))
>       (toXS 0.5) 0 (2 * pi)
>   setLineWidth (case True of
>                 _ | bsSelected bs -> 8
>                   | bsAllegiance bs == currentWizard -> 4
>                   | otherwise -> 2)
>   stroke


> drawGrid :: Double -> Double -> Render ()
> drawGrid squareWidth squareHeight = do
>   let toX a = a * squareWidth
>       toY b = b * squareHeight
>   setSourceRGB 0.2 0.2 0.2
>   --draw vertical gridlines
>   mapM_ (\x -> do
>                moveTo (toX x) 0
>                lineTo (toX x) (toY 10)) [1..14]
>   setLineWidth 1
>   stroke
>   --draw horizontal gridlines
>   mapM_ (\y -> do
>                      moveTo 0 (toY y)
>                      lineTo (toX 15) (toY y)) [1..9]
>   setLineWidth 1
>   stroke
>


================================================================================

= helper functions

read the board sprites relvar into the cache format

> readBoardSprites :: Connection -> IO (String,[BoardSprite])
> readBoardSprites conn = do
>    cw <- selectValueIf conn "select current_wizard from current_wizard_table" []
>    s <- selectTuplesC conn "select * from board_sprites" [] $
>                         \bs -> (BoardSprite (read $ lk "x" bs)
>                                             (read $ lk "y" bs)
>                                             (lk "sprite" bs)
>                                             (lk "allegiance" bs)
>                                             (lk "colour" bs)
>                                             (read $ lk "start_tick" bs)
>                                             (read $ lk "animation_speed" bs)
>                                             (read $ lk "selected" bs))
>    return (fromMaybe "" cw, s)


> readCurrentEffects :: Connection -> Int -> IO EffectsLine
> readCurrentEffects conn ticks = do
>   dbAction conn "update_effects_ticks" [show ticks]
>   bef <- selectTuples conn "select queuepos,subtype,x1,y1,x2,y2\n\
>                            \from current_board_beam_effects" []
>   sqef <- selectTuples conn "select queuepos,subtype,x1,y1\n\
>                             \from current_board_square_effects" []
>   sef <- selectTuples conn "select queuepos,subtype,sound_name\n\
>                            \from current_board_sound_effects" []
>   dbAction conn "update_effects_ticks" [show ticks]
>   let r = (map tToBE bef, map tToSqE sqef, map tToSE sef)
>   --putStrLn $ showEffectsLine r
>   return r
>     where
>       tToBE t = let l f = lk f t in
>                 let r = read . l in
>                 (l "subtype", r "x1", r "y1", r "x2", r "y2")
>       tToSqE t = let l f = lk f t in
>                  let r = read . l in
>                 (l "subtype", r "x1", r "y1")
>       tToSE t = let l f = lk f t in
>                 (l "subtype", l "sound_name")

> showEffectsLine :: EffectsLine -> String
> showEffectsLine (beamEffects, squareEffects, soundEffects) =
>   let s = show in
>   "----------" ++
>   if not $ null beamEffects
>     then "\nBeam effects:\n" ++
>     concatMap (\(subtype,x1,y1,x2,y2)
>                -> subtype ++ s x1 ++ "," ++ s y1 ++ ","
>                 ++ s x2 ++ "," ++ s y2 ++ "\n") beamEffects
>     else ""
>   ++
>   if not $ null squareEffects
>     then "Square effects:\n" ++
>     concatMap (\(subtype,x1,y1)
>                 -> subtype ++ s x1 ++ "," ++ s y1 ++ "\n") squareEffects
>     else ""
>   ++
>   if not $ null soundEffects
>     then "sound effects:\n" ++
>      concatMap (\(subtype,soundName)
>                 -> subtype ++ ": " ++ soundName ++ "\n") soundEffects
>     else ""


get the number of ticks since starting the program, so we can
work out which frame to show for each sprite, and cue the effects

> getTicks :: ClockTime -> IO Int
> getTicks startTicks = do
>   t <- getClockTime
>   let tdiff = diffClockTimes t startTicks
>       ps = ((tdPicosec tdiff * 25::Integer) `div`
>             ((10::Integer)^(12::Integer)))::Integer
>       f1 = fromIntegral (tdMin tdiff * 25 * 60) +
>              fromIntegral (tdHour tdiff * 25 * 60 * 60) +
>              fromIntegral (tdSec tdiff * 25) +  ps
>       b = fromInteger f1
>   return b::IO Int

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.BoardWidget." ++ l)

---------------

= new effects designs

spread, recede: grow piece from a point or shrink it
disappear: shower of sparks, fade out, fall apart animation
receive spell: shower of sparks
select: intensify highlight briefly, whilst selected keep highlighted in some way
unselect: just change highlight back to normal
walk, fly: move piece smoothly across board
attack: move piece half over target square, flash attacked creature in red
killed: morph to corpse or fade out/ shower of sparks
ranged attack: projectile is short line like in original, flash attacked creature in red
  fire: some sort of flame animation
new turn: announce "turn 3"
after win, move remaining creatures around randomly
dark power: multicoloured beam with multicoloured sparks, fade out, shower of sparks
lightning: draw a lighning bolt somehow, attacked flash red,etc.
magic bolt: ball which changes colour rapidly
chaos,law: darkness or brightness radiates out from wizard, larger/brighter for large versions
disbelieve: beam plus sparks
raise dead: beam plus reverse of dying anim without red
subversion: beam, flash allegiance highlights back an forth, if successful brighten allegiance highlight briefly
turmoil: everything rotates and bounces around with swirly stuff overlaid
monster: beam then grow from point, fade in, with sparks?
cast blob & fire: same as spread
wall, castle,trees: slide in from bottom: tree,wall draw bottom line and go up, others, draw top line at bottom then scroll up, draw next line
upgrades: sparks then morph?

resisted: list types: attack, lightning, etc.
