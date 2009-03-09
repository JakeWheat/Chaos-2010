> {-# OPTIONS  -cpp #-}

> module SoundLib (initPlayer, play, SoundPlayer) where

Disable sounds on windows since I can't get the haskell SDL library
compiled on windows.

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__) 

> import Graphics.UI.SDL.Mixer.Types
> import Graphics.UI.SDL.Mixer.General
> import Graphics.UI.SDL.Mixer.Channels
> import Graphics.UI.SDL.Mixer.Samples

#endif

> import Control.Concurrent
> import System.FilePath
> import Data.List
> import Data.Maybe
> import Control.Monad

> import Utils
> import Logging

> import qualified Data.Map as M

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) 

> type SoundPlayer = ()

#else

> type SoundPlayer = M.Map String Graphics.UI.SDL.Mixer.Types.Chunk

#endif

> initPlayer :: IO SoundPlayer
> initPlayer = lg "initPlayer" "" $ do

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) 

>   return ()

#else

>  catch (do
>          openAudio 44100 AudioS16Sys 2 4096
>          soundNames <- findAllFiles "sounds"
>          let soundNames' = soundNames
>          soundList <- mapM tryLoadWAV soundNames'
>          let properSounds = catMaybes $ map (\(a,b) -> case b of
>                                                        Just x -> Just (a,x)
>                                                        Nothing -> Nothing) $
>                zip (map takeBaseName soundNames') $ soundList
>          return $ M.fromList $ properSounds)
>        (\e -> do
>           putStrLn $ "error initialising sound: " ++ show e
>           return $ M.fromList $ [])

#endif


> play :: SoundPlayer -> String -> IO()
> play player soundName = do

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) 

>   return ()

#else

>   let w = M.lookup soundName player
>   case w of
>     Just w' -> do
>                  forkIO $ do
>                    --putStrLn $ "playing " ++ soundName
>                    playChannel (-1) w' 0

 >                    playMusic w' 2
 >                    threadDelay 2000000

>                    return ()
>                  return ()
>     Nothing -> do
>                  unless (player == M.empty) $
>                    putStrLn $ "tried to play non-existant sound: " ++ soundName
>                  return ()
>   return ()

#endif

 > main :: IO ()
 > main = do
 >  p <- initPlayer
 >  play p "sound1"

 >  openAudio 44100 AudioS16Sys 2 4096
 >  w <- loadMUS "sounds/sound1.mp3"
 >  playMusic w 1

 >  threadDelay 2000000

 >  closeAudio

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.SoundLib." ++ l)
