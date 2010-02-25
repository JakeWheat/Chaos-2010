
numty relational algebra thing, needs work

> module Games.Chaos2010.Tests.RelationalAlgebra where

> import Database.HaskellDB hiding (project)

> rename ((lo,ln):lps) hls = map rn $ rename lps hls
>                            where
>                              rn = hRenameLabel lo ln
> rename [] hls = hls

 > project lbs hls = map (project1 lbs) hls
 >                   where
 >                     project1 (l:ls) hl = l .=. (hl .!. l) .*. project1 ls hl
 >                     --project1 [] hl = emptyRecord

> restrict p hls = filter p hls

> extend ex hls = map (\r -> ex r .*. r) hls

> union hls hls1 = hls ++ hls1

> join hls hls1 = -- check for compatibility
>                 -- sort on common columns
>                 -- do the join
>                 undefined

join

union
not matching
sort tuples?
sort attributes?
matching
...