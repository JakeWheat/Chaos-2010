data PtypeTag
    deriving (Data.Typeable.Typeable)
type Ptype = Data.HList.FakePrelude.Proxy PtypeTag
ptype :: Ptype
ptype = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Ptype
    where showLabel _ = "ptype"
data AllegianceTag
    deriving (Data.Typeable.Typeable)
type Allegiance = Data.HList.FakePrelude.Proxy AllegianceTag
allegiance :: Allegiance
allegiance = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Allegiance
    where showLabel _ = "allegiance"
data TagTag
    deriving (Data.Typeable.Typeable)
type Tag = Data.HList.FakePrelude.Proxy TagTag
tag :: Tag
tag = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Tag
    where showLabel _ = "tag"
data XTag
    deriving (Data.Typeable.Typeable)
type X = Data.HList.FakePrelude.Proxy XTag
x :: X
x = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel X
    where showLabel _ = "x"
data YTag
    deriving (Data.Typeable.Typeable)
type Y = Data.HList.FakePrelude.Proxy YTag
y :: Y
y = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Y
    where showLabel _ = "y"
data FlyingTag
    deriving (Data.Typeable.Typeable)
type Flying = Data.HList.FakePrelude.Proxy FlyingTag
flying :: Flying
flying = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Flying
    where showLabel _ = "flying"
data SpeedTag
    deriving (Data.Typeable.Typeable)
type Speed = Data.HList.FakePrelude.Proxy SpeedTag
speed :: Speed
speed = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Speed
    where showLabel _ = "speed"
data AgilityTag
    deriving (Data.Typeable.Typeable)
type Agility = Data.HList.FakePrelude.Proxy AgilityTag
agility :: Agility
agility = Data.HList.FakePrelude.proxy
instance Data.HList.Record.ShowLabel Agility
    where showLabel _ = "agility"