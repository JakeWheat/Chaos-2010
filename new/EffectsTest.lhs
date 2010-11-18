ghc -threaded --make -Wall -hide-package FileManip -i:~/wd/chaos/trunk/ ~/wd/chaos/trunk/EffectsTest.lhs

Create sequences of effects to demo how the game will work:

spell casting: casting target effect, casting square effect, casted effect

summon monster:
beam from wizard to target square
swirl on target square
monster appears effect (maybe squished to bottom of square, then
animates to being unsquished)

> {-# LANGUAGE TupleSections #-}
> --import Control.Monad.State

> import Data.IORef

> import Control.Concurrent.Chan
> import Control.Concurrent
> import Control.Monad

> import Data.Maybe

> import Graphics.Rendering.Cairo

> import Resources
> import BoardSurface hiding (Beam)
> import qualified BoardSurface as BS
> import Gui
> import Input

-----------------------------------

> data Board = Board [(Maybe Int,Piece)] [(Maybe Int,Effect)] Cursor

> data Piece = Piece Int Int String -- animated sprite name
> data Effect = Beam Int Int Int Int
>             | Swirl Int Int
>             | StretchFromBottom Int Int String -- animated sprite name
>             | NullEffect -- used for timing
> data Cursor = Cursor Int Int



------------------------------------------------

the gui framework requires the draw to back buffer function, which is
just IORef Surface -> IO()

queue of board descriptions. This is added to when a series of effects
is wanted, then the final board will be the resultant board without
effects -> this simple queue replaces the update board function

drawtobackbuffer changes:

use an ioref board instead of a list of drawing instructions. when
called, this function uses some extra frame position info to either
create a new list of drawing instructions and then draws them, or if
the current board is expired because all the effects have run out,
then it resets the frame counters and gets the next board from the
board queue and draws it.


> getCursor :: Board -> Cursor
> getCursor (Board _ _ c) = c
> setCursor :: Board -> Cursor -> Board
> setCursor (Board s e _) c = Board s e c

> drawToBackBuffer :: (AnimatedSpriteMap,SpriteMap) -> IORef Int -> IORef Board -> Chan Board -> IORef Surface -> IO ()
> drawToBackBuffer (asm,sm) currentFrameRef br bchan ios = do
>   curFr <- getAndUpdateFc
>   getNewBoardIfNeeded curFr
>   fillInNothingFCs curFr
>   (w,h) <- backBufferDimensions
>   dis <- boardToDrawingInstructions (asm,sm) curFr br
>   s <- readIORef ios
>   withSimilarSurface s ContentColorAlpha w h $ \bs -> do
>     -- draw the new board
>     renderWith bs $ drawBoard sm dis w h
>     -- copy the board to the backbuffer
>     s' <- readIORef ios -- bit dodgy - the surface may have been finished
>                             -- during drawing, so get it again here
>     renderWith s' $ do
>       -- probably also a bit dodgy - could be updating the back buffer surface whilst
>       -- it is being used to update the screen
>       setSourceSurface bs 0 0
>       paint
>   where
>     backBufferDimensions = do
>       s <- readIORef ios
>       w <- imageSurfaceGetWidth s
>       h <- imageSurfaceGetHeight s
>       return (w,h)
>     fillInNothingFCs curFr = do
>       (Board sprs effs c) <- readIORef br
>       let repNothing Nothing = Just curFr
>           repNothing j@(Just _) = j
>       writeIORef br $ Board (map (\(f,s) -> (repNothing f, s)) sprs)
>                             (map (\(f,s) -> (repNothing f, s)) effs)
>                             c
>     getAndUpdateFc = do
>       curFr <- readIORef currentFrameRef
>       writeIORef currentFrameRef $ curFr + 1
>       return curFr
>     getNewBoardIfNeeded curFr = do
>       -- effects: 4 frames for now
>       -- if all effects are just, and the int + 4 < current frame, try to
>       -- get a new board from the chan
>       b@(Board _ effs _) <- readIORef br
>       let fs = map (\(f, _) -> f) effs
>           fns = sequence fs
>           valids = fmap (filter (\i -> i + sl > curFr)) fns
>       case valids of
>         -- all of them just, and all expired
>         Just x | null x -> do
>           ec <- isEmptyChan bchan
>           when (not ec) $ do
>             b1 <- readChan bchan
>             writeIORef br $ setCursor b1 $ getCursor b
>         _ -> return ()

> sl :: Int
> sl = 10

> boardToDrawingInstructions :: (AnimatedSpriteMap,SpriteMap) -> Int -> IORef Board -> IO [DrawingInstruction]
> boardToDrawingInstructions (asm,_sm) curFr boardRef = do
>   (Board sprs effs (Cursor cx cy)) <- readIORef boardRef
>   let dis = [DI 7 4
>             ,DI 0 0
>             ,DI 0 8
>             ,DI 14 0
>             ,DI 14 8
>             ]
>             ++ map (\(Just sfn, Piece x y s) -> Spr x y (getSprite sfn s)) sprs
>             ++ map drawEffect effs
>             ++ [Spr cx cy "cursor.0"]
>   return dis
>   where
>     getSprite sfn s = let (AnimatedSprite _ ss _) = fromMaybe (error $ "unknown anim sprite: " ++ s) $ lookup s asm
>                           l = length ss
>                           fn = (curFr - sfn) `mod` l
>                       in ss !! fn
>     drawEffect (_, Beam x0 y0 x1 y1) = BS.Beam x0 y0 x1 y1
>     drawEffect (Just sfn, Swirl x y) = Spr x y (getSprite sfn "magic_fire") --stand in effect
>     drawEffect (Just sfn, StretchFromBottom x y s) = SquishedSpr x y ((fi (curFr - sfn) + 0.1) / fi sl) (getSprite sfn s)
>     drawEffect _ = NullDI
>     fi = fromIntegral

-----------------------------------------------------------------

effects tests

> summonMonsterTest :: [Board]
> summonMonsterTest =
>   let b1 = addEffects [NullEffect] $ parseBoard
>            "W              \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               " [('W',"wizard0")]
>       b2 = addEffects [Beam 0 0 1 0
>                       ,Swirl 1 0]
>                       b1
>       b3 = addEffects [StretchFromBottom 1 0 "goblin"] b1
>       b4 = addPiecesDia
>            " G             \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               " [('G',"goblin")] b1
>   in [b1,b2,b3,b4]


citadel:
same as monster (or as wall?)

blob:
same as monster, but the blob grows from point in middle of square

fire:
same as monster

wall:
as monster, maybe draw the wall in line at a time from the bottom
for computers, cast very quickly overlapping

wood:
as monster, todo: animate growth. For magic wood, all the trees are cast at once

> magicWoodTest :: [Board]
> magicWoodTest =
>   let b1 = addEffects [NullEffect] $ parseBoard
>            "W              \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               \
>            \               " [('W',"wizard0")]
>       trees = map snd $ parseDiagram
>            " t t t         \
>            \               \
>            \t t t          \
>            \               \
>            \ t t           \
>            \               \
>            \               \
>            \               \
>            \               "
>       b2 = addEffects (concatMap (\(x,y) -> [Beam 0 0 x y
>                                             ,Swirl x y]) trees)
>                       b1
>       b3 = addEffects (map (\(x,y) -> StretchFromBottom x y "magic_tree") trees) b1
>       b4 = addPieces (map (\(x,y) -> Piece x y "magic_tree") trees) b1
>   in [b1,b2,b3,b4]


dark power:
beam to target square, flash the target with multiple colours, then if
success on monster have decay on that monster, if wizard maybe
multiple colours on all wizards pieces, then they all decay

what is decay: mixture of pixels being wiped out completely, with some
being alphaized until they are all gone

disbelieve:
beam to target square, swirl (or flash white? - or maybe nothing for
real, alpharise for imaginary), then decay

chaos
chaos: variated darkness sweeping over the board, or spreading out from wizard?
law: lightness

lightning:
several lines of lightning from source to target, changing, target flashing red

magic bolt:
fireball: flaming ball flying along with flames behind it slightly (or
all the way back to the source), target flashes red when hit

magic armour, etc.: swirl on wizard, then morph from current sprite to new sprite

shadow form: shadow form constant effect - varying alpha over the
sprite, casting is swirl then the effect just kicks in

raise dead: beam to target square, swirl, or corpse flashes?
morph from corpse to monster

subversion:
beam, swirl, mirror spin?, or flash?

turmoil:
each piece spins and moves to its new position, overlap the moves so
happens quickly

autonomous:
castle disappear: decay

fire spread: just use create for now? want different animations
depending on surrounding fires at some point

fire disappear: reverse of create?
blob spread: create for now
blob disappear: reverse of create

get new spell from magic wood: decay the tree to leave the wizard
behind it?

move phase
walking: piece moves smoothly between two squares
flying: as walking
walk attack: piece moves halfway into target square, target creature
flashes red
for creature death to corpse: morph to corpse graphic
death of undead: decay
death of wizard: creatures decay then wizard explodes somehow
fly attack: as with walk attack - if fly multiple squares, then still
moves all the way into half way into the target square
ranged attack: short beam to target, target flashes red
fire: as magic bolt (maybe change magic bolt later?), could have engulf effect on target?

non action graphics:

have piece sprites: each has a number of frames (maybe only 1), and
either animates forward and then repeat, or on cycle forward then
backward. Each sprite has a different animation speed.

squares valid, highlights, etc:
different set of cursors like with original
animation on cursors, cross hair lines?
cast target spell highlights
select piece highlights
walk, fly, walk attack, fly attack, ranged attack squares
piece alliegance highlights


overlay:
maybe have status line at bottom which doesn't overlap playing area
then have multiple overlays for other text:
history
spell book, spell help
cursor pieces details
turn information: turn no, alignment, spell chosen, phase, current wizard
available action list
ui help screen
new game
...
overlay manager which can layout multiple of these, change the
stacking, change the alphas

----------------------------------------------------

> parseBoard :: String -> [(Char,Sprite)] -> Board
> parseBoard s ks =
>   let ps = flip map (parseDiagram s) $ \(k, (x,y)) ->
>               maybe (error $ "key not found: " ++ [k]) ((Nothing,) . Piece x y)
>                 $ lookup k ks
>   in Board ps [] $ Cursor 0 0

> parseDiagram :: String -> [(Char,(Int,Int))]
> parseDiagram s = filter (\(c,_) -> c /= ' ')
>                  $ zip s [(x,y) | y <- [0..8], x <- [0..14]]

> addEffects :: [Effect] -> Board -> Board
> addEffects e1s (Board ps es c) = Board ps (map (Nothing,) e1s ++ es) c

> addPieces :: [Piece] -> Board -> Board
> addPieces ps (Board bps es c) = Board (bps ++ (map (Nothing,) ps)) es c

> addPiecesDia :: String -> [(Char,Sprite)] -> Board -> Board
> addPiecesDia s k (Board ps es c) =
>    let (Board ps' _ _) = parseBoard s k
>    in Board (ps' ++ ps) es c


----------------------------------------------------------------

> main :: IO ()
> main = do
>   sms <- loadPngs "data/sprites"
>   br <- newIORef $ Board [] [] $ Cursor 0 0
>   -- _ <- forkIO $ updateBoard br summonMonsterTest
>   inc <- newChan
>   _ <- forkIO $ handleInput br inc
>   frameCount <- newIORef 0
>   bchan <- newChan
>   forM_ summonMonsterTest $ writeChan bchan
>   forM_ magicWoodTest $ writeChan bchan
>   runGui inc (drawToBackBuffer sms frameCount br bchan)

> handleInput :: IORef Board -> Chan Input -> IO ()
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
>       atomicModifyIORef br $ \(Board sprs effs (Cursor cx cy)) ->
>         let cx' = min 14 $ max 0 $ cx + x
>             cy' = min 8 $ max 0 $ cy + y
>         in (Board sprs effs $ Cursor cx' cy'
>            ,())
