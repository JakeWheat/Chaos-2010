
> {-# LANGUAGE ScopedTypeVariables #-}
> module Games.Chaos2010.ConcreteUI.SDL.SDLGui (startSDL) where

> import Control.Concurrent (forkIO)

> import Control.Concurrent.Chan.Strict
> import Control.Monad

> import Games.Chaos2010.ConcreteUI.SDL.BoardWindow
> import Games.Chaos2010.UI.UITypes


> startSDL :: Chan Event -> Chan (String,WindowUpdate) -> [Window] -> IO()
> startSDL evChan guChan wins = do
>   forM_ wins $ \(Window title _ x y w h) -> do
>      _ <- forkIO $ startBoardWindow title x y w h evChan guChan
>      return ()
