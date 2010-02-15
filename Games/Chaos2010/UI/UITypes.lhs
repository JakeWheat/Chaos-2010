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
>                 --  ToggleButton String Bool ToggleButtonCallback
>                 | ToggleButtonGroup [(EventID,String)] EventID EventID --gid,currently selected
>                 | Button String EventID
>                   deriving (Eq,Show)

> type EventID = String

> data Event = Key String
>            | ButtonClick EventID
>            | ToggleButtonGroupClick EventID EventID
>              deriving (Eq,Show)
>
>
> data DBText = DBText (Database -> IO [MyTextItem])
> data Window = Window String Int Int Int Int
>               deriving (Eq,Show)

> instance NFData MyTextItem where
>     rnf (Text s) = rnf s `seq` ()
>     rnf (TaggedText tg t) = rnf tg `seq` rnf t `seq` ()
>     rnf (Image i) = rnf i `seq` ()
>     rnf (ToggleButtonGroup m e s) = rnf m `seq` rnf e `seq` rnf s `seq` ()
>     rnf (Button s e) = rnf s `seq` rnf e `seq` ()

> instance NFData Event where
>     rnf (Key s) = rnf s `seq` ()
>     rnf (ButtonClick e) = rnf e `seq` ()
>     rnf (ToggleButtonGroupClick e e1) = rnf e `seq` rnf e1 `seq` ()
