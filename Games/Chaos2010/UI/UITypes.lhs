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
>                 | ToggleButtonGroup [(EventID,String)] String EventID
>                 | Button String EventID
>                   deriving (Eq,Show)

> type EventID = String

> data Event = Key String
>            | ButtonClick String EventID
>            | ToggleButtonClick EventID EventID
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
>     rnf (ToggleButtonGroup m s e) = rnf m `seq` rnf s `seq` rnf e `seq` ()
>     rnf (Button s e) = rnf s `seq` rnf e `seq` ()

> instance NFData Event where
>     rnf (Key s) = s `seq` ()
>     rnf (ButtonClick s e) = s `seq` e `seq` ()
>     rnf (ToggleButtonClick e e1) = e `seq` e1 `seq` ()
