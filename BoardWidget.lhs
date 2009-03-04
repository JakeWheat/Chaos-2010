#! /usr/bin/env runghc

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
  the sprites

* reading a fresh copy of the board_sprites relvar is quite slow, so
  we prefer to avoid this unless it has changed, so the animation
  update code uses a cached copy of the boardsprites in an ioref

* because of the way the server code works, the client only sees an
  action has occured after the action has completed (from the action
  history). E.g if the action is a successful attack, the client sees
  the database before the attack is started, and then only sees the
  full results of the attack: the attacked piece has gone, and the
  attacking piece has been moved. But we want to overlay the attack
  effects on the previous board layout: with the attacked piece still
  there and the attacking piece in its starting position.

The main variables used are:

board_sprites relvar: this view contains the sprites for the pieces on
top, the board highlight sprites and the cursor sprite.

board_effects relvar: this table contains the effects that were
derived from the history entries created by the last action. It's used
as a queue, so when the board widget refresh sees enties in here, it
reads them out into local iorefs and clears this table.

shared iorefs in this code:
boardSpritesRef: this contains the cached board_sprites view
effectsRef: this contains the effects currently playing or queued to
be played soon
soundsRef: this contains the sounds queued to be played soon

We end up with two main drawing routines:

myDraw: this draws the locally saved copy of the board sprites, and
just animates them, so it works pretty quickly. It also handles
showing any effects briefly and triggering sound effects.

refresh: this reads the database, if there are effects waiting to be
played then it loads them up and skips loading a new copy of the board
sprites from the database, otherwise it updates the local copy of the
board sprites. In either case, it then calls the myDraw routine to
update the screen

If there are currently no effects then:

* timeout: to update the animations, a timeout is called every 100ms,
  this just calls myDraw

* refresh: this is currently called after every database action. This
  is a little inefficient but ensures that the board is kept up to
  date, so when the board sprites change, the board is updated pretty
  soon afterwoods.

When there are effects, the following sequence happens:

The sql code inserts rows into the effects table based on any new
history entries after an action is run. This updates the boardSpritesRef

The ui calls refresh in the usual way which calls the board widget
refresh function.

The refresh function checks the effects table, and getting some
results back, doesn't update the board sprites, so the displayed
sprites don't change from before the action was run. It updates the
effectsRef and soundsRef

The mydraw function is then called, it starts drawing the effects over
the existing board sprites, and triggering the sound effects. It just
updates the canvas once like usual.

The timeout continues to call mydraw which draws the effects and sound
effects until they are no longer fresh (each effect is typically ~ 0.5
secs long). As each sound is played it is removed from the queue, and
after an effect has been on for the required time it is removed from
the effectsRef. All this time, the client blocks ai and user initiated
actions, and the boardSpritesRef is not updated.

When mydraw removes the last effect, it restores the flag allowing the
ai and player to continue, and calls refresh to refresh the
boardSpritesRef and draw the new set of pieces.

It's slightly hacky, and might work better by the server giving the ui
control over the progression, e.g. so that action_attack initiates the
attack, adds the attack attempted history item then returns to the ui
which can play that effect then call an action_continue to get the
attacked piece dying and the pieces moving, this is similar to the way
the ai works atm.

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


> type BoardSpriteT = (Int, Int, [Char], Int, Int)
> type BoardSpritesCache = IORef [BoardSpriteT]
> type EffectsCache = IORef [M.Map [Char] [Char]]
> type SoundEffectsCache = IORef [(Int, [Char])]

================================================================================

= Ctor

> boardWidgetNew :: Connection -> SoundPlayer -> ColourList ->
>                   SpriteMap -> IO (Frame, IO())
> boardWidgetNew conn player _ spriteMap = do
>   --setup the gtk widgets
>   frame <- frameNew
>   canvas <- drawingAreaNew
>   containerAdd frame canvas
>   --setup the cache iorefs
>   bd <- readBoardSprites conn
>   boardSpritesRef <- newIORef bd
>   efc' <- selectTuples conn "select * from board_effects" []
>   effectsRef <- newIORef efc'
>   soundsRef <- newIORef ([]::[(Int,String)])

used to calculate how many ticks since the app was started, to time
the animations and effects:

>   startTicks <- getClockTime

hook things up the the expose event, this is how gtk triggers a
redraw, and doonexpose hooks this event up to the mydraw function

>   onExpose canvas (doOnExpose conn spriteMap player canvas startTicks
>                               boardSpritesRef effectsRef soundsRef)

update the board sprites 10 times a second to animate them

>   flip timeoutAdd 100 $ do
>     widgetQueueDrawArea canvas 0 0 2000 2000
>     return True

>   let refresh' = refresh conn canvas startTicks
>                          boardSpritesRef effectsRef soundsRef
>   return (frame, refresh')

================================================================================

= refresh

The refresh function updates the ioref caches from the database and
then calls mydraw.

> refresh :: Connection
>            -> DrawingArea
>            -> ClockTime
>            -> BoardSpritesCache
>            -> EffectsCache
>            -> SoundEffectsCache
>            -> IO ()
> refresh conn canvas startTicks boardSpritesRef effectsRef soundsRef =
>   lg "boardWidgetNew.refresh" "" $ do
>   ef <- checkForEffects conn startTicks effectsRef soundsRef
>   unless ef $ do

No effects, so update the board sprites cache

(first - if there are no effects update the flag that is used to
prevent the ai and player from continuing during an effect)

>     re <- selectValueIf conn "select running_effects\n\
>                              \from running_effects_table;" []
>     when ((isJust re) && (read $ fromJust re)) $
>          runSql conn "update running_effects_table\n\
>                      \set running_effects = false" []
>     bd1 <- readBoardSprites conn
>     writeIORef boardSpritesRef bd1

Now draw this stuff on screen

>   redraw canvas


This function checks the database for new effects and updates the
caches if there are some. It returns true if the effects cache is not
empty

> checkForEffects :: Connection
>                 -> ClockTime
>                 -> EffectsCache
>                 -> SoundEffectsCache
>                 -> IO Bool
> checkForEffects conn startTicks effectsRef soundsRef =
>   lg "boardWidgetNew.checkForEffects" "" $ do
>   efc <- selectTuples conn "select * from board_effects" []
>   when (length efc > 0) $ do
>     --set the flag to stop further player and ai actions
>     --whilst the effects are playing
>     runSql conn "update running_effects_table\n\
>                 \set running_effects = true" []
>     --load up the caches
>     --todo: clean this up, convert to stages, add new
>     --effects to existing effects
>     cf <- getFrames startTicks
>     let addStartFrame t = let sf = if lk "type" t == "beam"
>                                      then cf
>                                      else cf + 6
>                           in M.insert "start_frame" (show sf) t
>         removeSounds t = if lk "type" t == "sound"
>                            then Nothing
>                            else Just t
>     writeIORef effectsRef $ catMaybes $
>                map removeSounds $ map addStartFrame efc
>     let soundEffects = map (\t -> if lk "type" t /= "square"
>                                     then (cf, lk "sound" t)
>                                     else (cf + 12, lk "sound" t)) efc
>     writeIORef soundsRef soundEffects
>     runSql conn "delete from board_effects" []

tell the caller whether there are effects in the cache or not

>   efcE <- readIORef effectsRef
>   return $ length efcE > 0


================================================================================

= mydraw and drawing routines

This is the code that actually draws the canvas which is then rendered
to show the player

> myDraw :: Connection
>           -> SpriteMap
>           -> SoundPlayer
>           -> ClockTime
>           -> BoardSpritesCache
>           -> EffectsCache
>           -> SoundEffectsCache
>           -> Double
>           -> Double
>           -> Render ()
> myDraw conn spriteMap player startTicks boardSpritesRef effectsRef soundsRef w h = do
>   --make the background black
>   setSourceRGB 0 0 0
>   paint

work out the size of each square in cairo co-ords

>   let boardWidth = 15
>       boardHeight = 10
>       squareWidth = (w / boardWidth) ::Double
>       squareHeight = (h / boardHeight) ::Double
>
>   drawGrid squareWidth squareHeight

assume sprites are 64x64
get our scale factors so that the sprites are drawn at the same size
as the grid squares

>   let sw = 64
>       sh = 64
>       scaleX = squareWidth / sw
>       scaleY = squareHeight / sh
>

use a scale transform on the cairo drawing surface to scale the
sprites. This seems a bit backwards since we have to generate new toX
and toY functions which take into account the changed scale factor.

>   scale scaleX scaleY
>   let toXS a = a * squareWidth / scaleX
>       toYS b = b * squareHeight / scaleY

>   cf <- liftIO $ getFrames startTicks

>   drawSprites cf spriteMap toXS toYS boardSpritesRef
>   liftIO $ playSounds player cf soundsRef
>   drawEffects conn spriteMap cf boardSpritesRef effectsRef toXS toYS

== drawing helpers

> playSounds :: SoundPlayer -> Int -> SoundEffectsCache -> IO ()
> playSounds player cf soundsRef = do
>   snds <- readIORef soundsRef
>   when (length snds > 0) $ do
>     leftSnds <- mapM (\(t,s) -> if t <= cf
>                                   then do
>                                        play player s
>                                        return Nothing
>                                   else return $ Just (t,s)) snds
>     writeIORef soundsRef $ catMaybes leftSnds

> drawSprites :: Int
>             -> SpriteMap
>             -> (Double -> Double)
>             -> (Double -> Double)
>             -> BoardSpritesCache
>             -> Render ()
> drawSprites cf spriteMap toXS toYS boardSpritesRef = do

create a helper function to draw a sprite at board position x,y
identifying the sprite by name, hiding all that tedious map lookup
stuff

Draw the board sprites from the saved board

>   bd2 <- liftIO $ readIORef boardSpritesRef
>   mapM_ (uncurry5 (drawAt spriteMap toXS toYS cf)) bd2

> drawEffects :: Connection
>             -> SpriteMap
>             -> Int
>             -> BoardSpritesCache
>             -> EffectsCache
>             -> (Double -> Double)
>             -> (Double -> Double)
>             -> Render ()
> drawEffects conn spriteMap cf boardSpritesRef effectsRef toXS toYS = do
>   efs <- liftIO $ readIORef effectsRef
>   when (length efs > 0) $ do
>     let drawEffectLine x1 y1 x2 y2 = do
>           setSourceRGB 0.7 0.7 0.7
>           moveTo (toXS $ x1 + 0.5) (toYS $ y1 + 0.5)
>           lineTo (toXS $ x2 + 0.5) (toYS $ y2 + 0.5)
>           setLineWidth 10
>           stroke

>     let drawEffectSquare x1 y1 = drawAt spriteMap toXS toYS cf x1 y1 "effect_attack" 0 250

>             --remove expired effects
>     let efs1 = filter (\t -> (read (lk "start_frame" t)) + 6 >= cf) efs
>             --liftIO $ putStrLn $ show (length efs1) ++ "effects still good"
>             --liftIO $ putStrLn $ show cf ++ " current frame"
>             --mapM_ (\t -> liftIO $ putStrLn (lk "start_frame" t)) efs
>     forM_ efs1 $ \t -> do
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
>     --update the list of effects to remove the expired ones
>     liftIO $ writeIORef effectsRef efs1
>     -- if there are no more effects then refresh the sprites from the database
>     when (length efs1 == 0)$ do
>       bd1 <- liftIO $ readBoardSprites conn
>       liftIO $ writeIORef boardSpritesRef bd1


> drawAt :: SpriteMap
>        -> (Double -> Double)
>        -> (Double -> Double)
>        -> Int
>        -> Int
>        -> Int
>        -> String
>        -> Int
>        -> Int
>        -> Render ()
> drawAt spriteMap toXS toYS cf x y --current frame, grid x, grid y
>        spriteName sf as = do -- sprite start frame, animation speed
>   let p = safeMLookup "board widget draw" spriteName spriteMap
>       (_,_,img) = p
>       f = ((cf - sf) `div` as) `mod` length img
>   setSourceSurface (img !! f) (toXS $ fromIntegral x) (toYS $ fromIntegral y)
>   paint



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


> readBoardSprites :: Connection -> IO [BoardSpriteT]
> readBoardSprites conn =
>           selectTuplesC conn "select * from board_sprites" [] $
>                         \bs -> (read $ lk "x" bs::Int,
>                                 read $ lk "y" bs::Int,
>                                 lk "sprite" bs,
>                                 read $ lk "start_frame" bs::Int,
>                                 read $ lk "animation_speed" bs::Int)

get the number of frames since starting the program, so we can
work out which frame to show for each sprite, and cue the effects

> getFrames :: ClockTime -> IO Int
> getFrames startTicks = do
>   t <- getClockTime
>   let tdiff = diffClockTimes t startTicks
>       ps = ((tdPicosec tdiff * 25::Integer) `div`
>             ((10::Integer)^(12::Integer)))::Integer
>       f1 = fromIntegral (tdMin tdiff * 25 * 60) +
>              fromIntegral (tdHour tdiff * 25 * 60 * 60) +
>              fromIntegral (tdSec tdiff * 25) +  ps
>       b = fromInteger f1
>   return b::IO Int

This posts an event to the gtk queue which triggers the onexpose,
which then triggers the mydraw routine, which apparently how you do
this sort of thing in gtk

> redraw :: DrawingArea -> IO ()
> redraw canvas = do
>   win <- widgetGetDrawWindow canvas
>   reg <- drawableGetClipRegion win
>   drawWindowInvalidateRegion win reg True
>   drawWindowProcessUpdates win True

gtk red tape: on expose routes (re)draw events coming from gtk to the
my draw function - these redraws can be trigged by the redraw function
or by gtk in response to windows being moved around or resized

> doOnExpose :: Connection
>            -> SpriteMap
>            -> SoundPlayer
>            -> DrawingArea
>            -> ClockTime
>            -> BoardSpritesCache
>            -> EffectsCache
>            -> SoundEffectsCache
>            -> t
>            -> IO Bool
> doOnExpose conn spriteMap player canvas startTicks
>            boardSpritesRef soundsRef effectsRef _ =
>   lg "boardWidgetNew.doOnExpose" "" $ do
>   (w,h) <- widgetGetSize canvas
>   drawin <- widgetGetDrawWindow canvas
>   renderWithDrawable drawin
>                      (myDraw conn spriteMap player startTicks
>                       boardSpritesRef soundsRef effectsRef
>                       (fromIntegral w) (fromIntegral h))
>   --The following line use to read
>   --return (eventSent x))
>   --but that doesn't compile anymore, so just bodged it.
>   return True




> lg :: String -> String -> IO c -> IO c
> lg l m = Logging.pLog ("chaos.BoardWidget." ++ l) m
