
> module SoundLib (initPlayer, play) where

> import Graphics.UI.SDL.Mixer.Music
> import Graphics.UI.SDL.Mixer.Types

> import Control.Concurrent

> import System.FilePath

> import Graphics.UI.SDL.Mixer.General
> import Graphics.UI.SDL.Mixer.Channels
> import Graphics.UI.SDL.Mixer.Samples
> import Data.List
> import Data.Maybe
> import Control.Monad

> import Utils

> import qualified Data.Map as M

> type SoundPlayer = M.Map String Graphics.UI.SDL.Mixer.Types.Chunk

> initPlayer :: IO SoundPlayer
> initPlayer = do
>  openAudio 44100 AudioS16Sys 2 4096
>  soundNames <- findAllFiles "sounds"
>  let soundNames' = soundNames

 >  forM_ soundNames' $ \f -> do
 >                putStrLn $ "file: " ++ f

 >  soundList <- mapM loadMUS soundNames
 >  return $ M.fromList $ zip (map takeBaseName soundNames) soundList

>  soundList <- mapM tryLoadWAV soundNames'

 >  forM_ (zip (map takeBaseName soundNames') $ soundList)
 >        (\(a,b) -> putStrLn $ a ++ show (isJust b))

>  let properSounds = catMaybes $ map (\(a,b) -> case b of
>                                        Just x -> Just (a,x)
>                                        Nothing -> Nothing) $
>                     zip (map takeBaseName soundNames') $ soundList
>  return $ M.fromList $ properSounds


> play :: SoundPlayer -> String -> IO()
> play player soundName = do
>   let w = M.lookup soundName player
>   case w of
>     Just w' -> do
>                  forkIO $ do
>                    putStrLn $ "playing " ++ soundName
>                    playChannel (-1) w' 0

 >                    playMusic w' 2
 >                    threadDelay 2000000

>                    return ()
>                  return ()
>     Nothing -> do
>                  putStrLn $ "tried to play non-existant sound: " ++ soundName
>                  return ()
>   return ()

 > main :: IO ()
 > main = do
 >  p <- initPlayer
 >  play p "sound1"

 >  openAudio 44100 AudioS16Sys 2 4096
 >  w <- loadMUS "sounds/sound1.mp3"
 >  playMusic w 1

 >  threadDelay 2000000

 >  closeAudio

