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
>                 | ToggleButtonGroup [(String,String)] String (String -> IO()) --name,label, name of current select, callback
>                 | Button String (IO())

 > type EventID = String

> data Event = Key String
>            | Callback (IO())

> data SpriteGrid = SpriteGrid Int Int [(Int,Int,String)]

> data DBSpriteGrid = DBSpriteGrid (Database -> IO SpriteGrid)
>
>
> data DBText = DBText (Database -> IO [MyTextItem])

dbtext: don't like this because it's a function, want something more
declarative.

perhaps:
data Text = ConstText [MyTextItem]
          | DBText (query ast) (ui ast)
          | ... other data sources?
in the ui ast we just have data references e.g. something like
data Field = SField String
           | MBoolField (Maybe Bool)
           | et.

we can treat these as the wrapped data type, e.g. append an SField
String as if it was a string

the we have a processor which does:
Text -> (Database - >IO [MyTextItem])
and the field accesses are typechecked at this point

> data Window = Window String WidgetType Int Int Int Int
>               deriving (Eq,Show)

> data WidgetType = WText
>                 | WSpriteGrid
>                   deriving (Eq,Show)

> data WindowUpdate = WUText [MyTextItem]
>                   | WUSpriteGrid SpriteGrid

> instance NFData WindowUpdate where
>     rnf (WUText is) = rnf is `seq` ()
>     rnf (WUSpriteGrid sg) = rnf sg `seq` ()


> instance NFData MyTextItem where
>     rnf (Text s) = rnf s `seq` ()
>     rnf (TaggedText tg t) = rnf tg `seq` rnf t `seq` ()
>     rnf (Image i) = rnf i `seq` ()
>     rnf (ToggleButtonGroup m s _) = rnf m `seq` rnf s `seq` ()
>     rnf (Button s _) = rnf s `seq` ()

> instance NFData Event where
>     rnf (Key s) = rnf s `seq` ()
>     rnf (Callback _) = ()

> instance NFData SpriteGrid where
>     rnf (SpriteGrid x y l) = rnf x `seq` rnf y `seq` rnf l `seq` ()
