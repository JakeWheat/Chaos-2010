
> module ChaosTypes (ColourList, SpriteMap) where

> import Graphics.UI.Gtk
> import qualified Data.Map as M
> import Graphics.Rendering.Cairo


> type ColourList = [(String,Color)]

> type SpriteMap = M.Map String ([Pixbuf], [Pixbuf], [Surface])
