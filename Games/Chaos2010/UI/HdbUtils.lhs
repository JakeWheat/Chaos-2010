> module Games.Chaos2010.UI.HdbUtils where

> import Database.HaskellDB
> import Database.HaskellDB.Database

> qdb :: (GetRec er vr) =>
>        Database -> Query (Rel (Record er)) -> (Record vr -> b) -> IO [b]
> qdb db t r = query db t >>= return . map r
>
> jn :: Expr (Maybe String) -> Expr String
> jn = fromNull (constant "")
>
> mv :: Maybe String -> String
> mv = maybe "" id

> smn :: Maybe Int -> String
> smn = show . maybe 0 id



