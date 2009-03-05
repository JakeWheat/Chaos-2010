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

> type SoundEffect = (String,String)
> type SquareEffect = (String,Int,Int)
> type BeamEffect = (String,Int,Int,Int,Int)
> type EffectsLine = (Int
>                    ,[BeamEffect]
>                    ,[SquareEffect]
>                    ,[SoundEffect])

> type EffectsCacheType = (Int,[EffectsLine])

> type EffectsCache = IORef EffectsCacheType


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
>   effectsRef <- newIORef ((0,[])::EffectsCacheType)

clear the effects tables so we don't have a load of effects waiting to
run when the app is started

>   runSql conn "delete from board_sound_effects" []
>   runSql conn "delete from board_square_effects" []
>   runSql conn "delete from board_beam_effects" []

used to calculate how many ticks since the app was started, to time
the animations and effects:

>   startTicks <- getClockTime

hook things up the the expose event, this is how gtk triggers a
redraw, and doonexpose hooks this event up to the mydraw function

>   let refresh' = refresh conn canvas boardSpritesRef effectsRef
>   onExpose canvas (doOnExpose refresh' spriteMap player canvas startTicks
>                               boardSpritesRef effectsRef)

update the board sprites 10 times a second to animate them

>   flip timeoutAdd 100 $ do
>     widgetQueueDrawArea canvas 0 0 2000 2000
>     return True

>   return (frame, refresh')

================================================================================

= refresh

The refresh function updates the ioref caches from the database and
then calls mydraw.

> refresh :: Connection
>            -> DrawingArea
>            -> BoardSpritesCache
>            -> EffectsCache
>            -> IO ()
> refresh conn canvas boardSpritesRef effectsRef =
>   lg "boardWidgetNew.refresh" "" $ do
>   ef <- checkForEffects conn effectsRef
>   unless ef $ do

No effects, so update the board sprites cache

(first - if there are no effects update the flag that is used to
prevent the ai and player from continuing during an effect)

>     re <- selectValueIf conn "select running_effects\n\
>                              \from running_effects_table;" []
>     when (isJust re && read (fromJust re)) $
>          runSql conn "update running_effects_table\n\
>                      \set running_effects = false" []
>     bd1 <- readBoardSprites conn
>     writeIORef boardSpritesRef bd1

Now draw this stuff on screen

>   redraw canvas


This function checks the database for new effects and updates the
caches if there are some. It returns true if the effects cache is not
empty.

TODO overview of the effects queue and how it works

> checkForEffects :: Connection
>                 -> EffectsCache
>                 -> IO Bool
> checkForEffects conn effectsRef =
>   lg "boardWidgetNew.checkForEffects" "" $ do
>   sef <- selectTuples conn "select queuepos,subtype,sound_name\n\
>                            \from board_sound_effects" []
>   sqef <- selectTuples conn "select queuepos,subtype,x1,y1\n\
>                             \from board_square_effects" []
>   bef <- selectTuples conn "select queuepos,subtype,x1,y1,x2,y2\n\
>                            \from board_beam_effects" []
>

>   unless (null sef && null sqef && null bef) $ do
>     --putStrLn "found new effects"
>     --set the flag to stop further player and ai actions
>     --whilst the effects are playing
>     runSql conn "update running_effects_table\n\
>                 \set running_effects = true" []

clean up the effects which are now being put into the cache

>     runSql conn "delete from board_sound_effects" []
>     runSql conn "delete from board_square_effects" []
>     runSql conn "delete from board_beam_effects" []

load up the caches
get the list of unique qps
each unique qp will correspond to a line in the effects queue

>     let qps = (sort . nub)  ((map (\t -> read $ lk "queuepos" t) sef) ++
>                            map (\t -> read $ lk "queuepos" t) sqef ++
>                            map (\t -> read $ lk "queuepos" t) bef) :: [Int]
>     --convert sql tuples to haskell tuples
>     let tToBE t = let l f = lk f t in
>                   let r = read . l in
>                   (l "subtype", r "x1", r "y1", r "x2", r "y2")
>     let tToSqE t = let l f = lk f t in
>                    let r = read . l in
>                   (l "subtype", r "x1", r "y1")
>     let tToSE t = let l f = lk f t in
>                   (l "subtype", l "sound_name")
>     -- filter a list selecting the tuples which match queuePos
>     let getByQp qp = filter (\t -> read (lk "queuepos" t) == qp)

take the three tables from the db and convert to a list of effects
lines, so each beam,square and sound effect with the same qp will
appear in the same line and be triggered together

>     let newEffects = for qps (\qp -> (qp
>                                      ,map tToBE $ getByQp qp bef
>                                      ,map tToSqE $ getByQp qp sqef
>                                      ,map tToSE $ getByQp qp sef
>                                      ))
>     (pos,currentEffects) <- readIORef effectsRef
>     --putStrLn $ "new effects:\n" ++ concatMap showEffectsLine newEffects
>     --putStrLn $ "1set pos to " ++ show pos
>     writeIORef effectsRef (if null currentEffects then 0 else pos,
>                            currentEffects ++ newEffects)
>     return()

tell the caller whether there are effects in the cache or not

>   (_,effs) <- readIORef effectsRef
>   --putStrLn $ "effects queue has " ++ (show $ length effs)
>   return $ not $ null effs

> showEffectsLine :: EffectsLine -> String
> showEffectsLine (qp, beamEffects, squareEffects, soundEffects) =
>   let s = show in
>   "----------\n" ++ show qp ++
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

================================================================================

= mydraw and drawing routines

This is the code that actually draws the canvas which is then rendered
to show the player

> myDraw :: IO()
>        -> SpriteMap
>        -> SoundPlayer
>        -> ClockTime
>        -> BoardSpritesCache
>        -> EffectsCache
>        -> Double
>        -> Double
>        -> Render ()
> myDraw refresh' spriteMap player startTicks
>        boardSpritesRef effectsRef w h = do
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
>   cf <- liftIO $ getTicks startTicks
>   drawSprites cf spriteMap toXS toYS boardSpritesRef
>   runEffects refresh' spriteMap cf player boardSpritesRef effectsRef toXS toYS

== drawing helpers

Draw the board sprites from the saved board

> drawSprites :: Int
>             -> SpriteMap
>             -> (Double -> Double)
>             -> (Double -> Double)
>             -> BoardSpritesCache
>             -> Render ()
> drawSprites cf spriteMap toXS toYS boardSpritesRef = do
>   bd2 <- liftIO $ readIORef boardSpritesRef
>   mapM_ (uncurry5 (drawAt spriteMap toXS toYS cf)) bd2

process the effects queue, remove any effects which have finished,
play any new sounds, draw any current visual effects

> runEffects :: IO()
>            -> SpriteMap
>            -> Int
>            -> SoundPlayer
>            -> BoardSpritesCache
>            -> EffectsCache
>            -> (Double -> Double)
>            -> (Double -> Double)
>            -> Render ()
> runEffects refresh' spriteMap cf player _ effectsRef toXS toYS = do
>   (pos',currentEffectsQueue) <- liftIO $ readIORef effectsRef
>   unless (null currentEffectsQueue) $ do

check if the pos hasn't been set, if not, set it and play the first
set of sounds

>     when (pos' == 0) $ do
>       --liftIO $ putStrLn "init effects"
>       playNewSounds currentEffectsQueue
>       --liftIO $ putStrLn $ "2set pos to " ++ show cf
>       liftIO $ writeIORef effectsRef (cf, currentEffectsQueue)

>     (pos,_) <- liftIO $ readIORef effectsRef

see if the head of the currenteffectsqueue has expired (they last for
12 ticks)

>     when (cf > pos + 12) $ do
>       --clear the current row of effects and play the sounds for the next one
>       --liftIO $ putStrLn $ "next effects: " ++ show cf ++ " > " ++ show pos ++" + 24"
>       let newCurrentEffectsQueue = tail currentEffectsQueue
>       --liftIO $ putStrLn $ "3set pos to " ++ show cf
>       liftIO $ writeIORef effectsRef (cf, newCurrentEffectsQueue)
>       playNewSounds newCurrentEffectsQueue

display the visual effects

>     (_,vCurrentEffectsQueue) <- liftIO $ readIORef effectsRef
>     unless (null vCurrentEffectsQueue) (do
>       let (qp,beamEffects,squareEffects,_):_ = vCurrentEffectsQueue
>       --liftIO $ putStrLn $ "drawing effects for " ++ show qp
>       mapM_ drawBeamEffect beamEffects
>       mapM_ drawSquareEffect squareEffects)

refresh the board sprites if no more effects

>     (_,remainingEffects) <- liftIO $ readIORef effectsRef
>     when (null remainingEffects) $ liftIO refresh'

helpers

>     where

play all the sounds from the head of the effects queue

>       playNewSounds ((_,_,_,currentSounds) : _) =
>         liftIO $ mapM_ (play player . snd)  currentSounds
>       playNewSounds [] = return ()

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
>         drawAt spriteMap toXS toYS cf x1 y1 "effect_attack" 0 250


helper function to draw a sprite at board position x,y identifying the
sprite by name, hiding all that tedious map lookup stuff

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
> drawAt spriteMap toXS toYS cf x y --current ticks, grid x, grid y
>        spriteName sf as = do -- sprite start tick, animation speed
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
>                                 read $ lk "start_tick" bs::Int,
>                                 read $ lk "animation_speed" bs::Int)

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

> doOnExpose :: IO()
>            -> SpriteMap
>            -> SoundPlayer
>            -> DrawingArea
>            -> ClockTime
>            -> BoardSpritesCache
>            -> EffectsCache
>            -> t
>            -> IO Bool
> doOnExpose refresh' spriteMap player canvas startTicks
>            boardSpritesRef effectsRef _ =
>   lg "boardWidgetNew.doOnExpose" "" $ do
>   (w,h) <- widgetGetSize canvas
>   drawin <- widgetGetDrawWindow canvas
>   renderWithDrawable drawin
>                      (myDraw refresh' spriteMap player startTicks
>                       boardSpritesRef effectsRef
>                       (fromIntegral w) (fromIntegral h))
>   --The following line use to read
>   --return (eventSent x))
>   --but that doesn't compile anymore, so just bodged it.
>   return True




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
