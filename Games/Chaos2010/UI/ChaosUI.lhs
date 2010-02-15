Copyright 2010 Jake Wheat

The top level description of the entire user interface, which is a
list of windows.

> module Games.Chaos2010.UI.ChaosUI where

> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.UI.InfoWidget
> import Games.Chaos2010.UI.SpellBookWidget
> import Games.Chaos2010.UI.NewGameWidget
> import Games.Chaos2010.UI.BoardWidget
> import Games.Chaos2010.UI.ActionHistoryWidget

> chaosUI :: [Window]
> chaosUI = [Window "Info" 0 371 579 213 infoWidget
>           ,Window "Spell Book" 587 28 268 556 spellBookWidget
>           ,Window "New Game" 514 27 500 500 newGameWidget
>           ,Window "Board" 99 28 480 320 boardWidget
>           ,Window "Action History" 843 28 429 556 actionHistoryWidget]
