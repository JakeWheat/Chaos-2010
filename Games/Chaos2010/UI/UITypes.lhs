Copyright 2010 Jake Wheat

data types for the abstract ui

> module Games.Chaos2010.UI.UITypes where
>
> import Database.HaskellDB
> import Control.DeepSeq
>
> data MyTextItem = Text String
>                 | TaggedText [String] String
>                 | Image String
>                 {- | ToggleButton String Bool ToggleButtonCallback
>                 | ToggleButtonGroup [String] String ToggleGroupCallback
>                 | Button String ButtonCallback-}
>
> type ToggleButtonCallback = Bool -> IO()
> type ToggleGroupCallback = String -> IO()
> type ButtonCallback = IO()
>
> data DBText = DBText (Database -> IO [MyTextItem])
> data Window = Window String Int Int Int Int

> instance NFData MyTextItem where
>     rnf (Text s) = rnf s `seq` ()
>     rnf (TaggedText tg t) = rnf tg `seq` rnf t `seq` ()
>     rnf (Image i) = rnf i `seq` ()
