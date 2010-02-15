Copyright 2010 Jake Wheat

data types for the abstract ui

> module Games.Chaos2010.UI.UITypes where

> import Database.HaskellDB


> data MyTextItem = Text String
>                 | TaggedText String [String]
>                 | Image String
>                 | ToggleButton String Bool ToggleButtonCallback
>                 | ToggleButtonGroup [String] String ToggleGroupCallback
>                 | Button String ButtonCallback

> type ToggleButtonCallback = Bool -> IO()
> type ToggleGroupCallback = String -> IO()
> type ButtonCallback = IO()


> data DBText = DBText (Database -> IO [MyTextItem])
> data Window = Window String Int Int Int Int DBText