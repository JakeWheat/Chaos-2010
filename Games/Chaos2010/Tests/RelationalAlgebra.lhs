
numty relational algebra thing, needs work

> {-# LANGUAGE FlexibleContexts #-}
> module Games.Chaos2010.Tests.RelationalAlgebra where

> import Database.HaskellDB hiding (project)

> rename :: (HRLabelSet (HCons (LVPair t1 v) t21),
>            HEq t t1 b,
>            HasField' b t (HCons (LVPair t1 v) t21) v,
>            HEq t1 t b2,
>            HMemberM' b2 t1 (HCons t HNil) b1,
>            H2ProjectByLabels'
>            b1 (HCons t HNil) (HCons (LVPair t1 v) t21) t2 t21) =>
>           [(t, t1)]
>        -> [Record (HCons (LVPair t1 v) t21)]
>        -> [Record (HCons (LVPair t1 v) t21)]
> rename ((lo,ln):lps) hls = map rn $ rename lps hls
>                            where
>                              rn = hRenameLabel lo ln
> rename [] hls = hls

 > project lbs hls = map (project1 lbs) hls
 >                   where
 >                     project1 (l:ls) hl = l .=. (hl .!. l) .*. project1 ls hl
 >                     --project1 [] hl = emptyRecord

> restrict :: (Record a -> Bool)
>          -> [Record a]
>          -> [Record a]
> restrict p hls = filter p hls

> extend :: (HExtend e a l') =>
>           (a -> e) -> [a] -> [l']
> extend ex hls = map (\r -> ex r .*. r) hls


> union :: [Record a] -> [Record a] -> [Record a]
> union hls hls1 = hls ++ hls1

> update :: (Record a -> Record a)
>           -> (Record a -> Bool)
>           -> [Record a]
>           -> [Record a]
> update u w hls = map u1 hls
>     where
>       u1 = \r -> if w r
>                  then u r
>                  else r


 > join hls hls1 = -- check for compatibility
 >                 -- sort on common columns
 >                 -- do the join
 >                 undefined

union
not matching
matching
compose
aggregates
aggregates and nested relations
summarize
group, ungroup
wrap, unwrap
relation comparison