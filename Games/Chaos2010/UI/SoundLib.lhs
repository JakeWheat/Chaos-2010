> {-# OPTIONS  -cpp #-}

> module Games.Chaos2010.UI.SoundLib (initPlayer, play, SoundPlayer) where

> import Graphics.UI.SDL.Mixer.Types
> import Graphics.UI.SDL.Mixer.General
> import Graphics.UI.SDL.Mixer.Channels
> import Graphics.UI.SDL.Mixer.Samples

> import Control.Concurrent
> import System.FilePath
> import Data.List
> import Data.Maybe
> import Control.Monad
> import Paths_Chaos2010

> import Games.Chaos2010.Utils
> import qualified Games.Chaos2010.Misc.Logging as Logging

> import qualified Data.Map as M

> type SoundPlayer = M.Map String Graphics.UI.SDL.Mixer.Types.Chunk

> initPlayer :: IO SoundPlayer
> initPlayer = lg "initPlayer" "" $ do

>  catch (do
>          openAudio 44100 AudioS16Sys 2 4096
>          sFolder <- getDataFileName "sounds"
>          soundNames <- findAllFiles sFolder
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


> play :: SoundPlayer -> String -> IO()
> play player soundName = do

>   let w = M.lookup soundName player
>   case w of
>     Just w' -> do
>                  forkIO $ do
>                    --putStrLn $ "playing " ++ soundName
>                    playChannel (-1) w' 0
>                    return ()
>                  return ()
>     Nothing -> do
>                  unless (player == M.empty) $
>                    putStrLn $ "tried to play non-existant sound: " ++ soundName
>                  return ()
>   return ()

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.SoundLib." ++ l)
