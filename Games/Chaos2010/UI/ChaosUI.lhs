Copyright 2010 Jake Wheat

The top level description of the entire user interface, which is a
list of windows.

> module Games.Chaos2010.UI.ChaosUI where

> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.UI.NewInfoWidget

> chaosUI :: [Window]
> chaosUI = [Window "Info widget" 0 0 1000 500 infoWidget]

