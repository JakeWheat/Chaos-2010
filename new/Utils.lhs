
> module Utils where

> partition :: (Eq k) => (b -> k) -> [b] -> [(k, [b])]
> partition f tvs =
>     partition' tvs []
>     where
>       partition' vs acc = foldr (\v acc' -> insertWith (++) (f v) [v] acc') acc vs

> insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
> insertWith cmb k v ((k1,v1):as) =
>     if k == k1
>     then (k1,cmb v1 v) : as
>     else (k1,v1) : insertWith cmb k v as
> insertWith _ k v [] = [(k,v)]
