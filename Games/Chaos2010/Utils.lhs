> {-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, FlexibleContexts #-}
> module Games.Chaos2010.Utils where
> import System.Time
> import Database.HaskellDB
> import Control.Exception
> import Database.HaskellDB.Database
> import Test.HUnit


> insertWith :: Eq k => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
> insertWith ac k v m =
>     case lookup k m of
>       Nothing -> m ++ [(k,v)]
>       Just v' -> let nv = ac v' v
>                  in map (\p@(k1,_) -> if k1 == k
>                                       then (k1,nv)
>                                       else p) m

> safeLookup :: (Show a, Eq a) => String -> a -> [(a, b)] -> b
> safeLookup errMsg key lkp =
>     case lookup key lkp of
>       Just x -> x
>       Nothing -> error $ errMsg ++ " missing key: " ++ show key

> whenA1 :: IO a -> (a -> Bool) -> IO () -> IO ()
> whenA1 feed cond f = (cond `liftM` feed) >>= flip when f

> dropItemN :: [a] -> Int -> [a]
> dropItemN [] _ = []
> dropItemN (x:xs) i = if i == 0
>                        then xs
>                        else x: dropItemN xs (i - 1)

> data CountTag deriving Typeable
> type Count = Proxy CountTag

> xcount :: Count
> xcount = proxy

> getCount :: Database -> Query t -> IO Int
> getCount db t = do
>   rel <- query db $ do
>                   _ <- t
>                   project $ xcount .=. count (constant (1::Int))
>                             .*. emptyRecord
>   return $ getSingleValue rel
>   --return $ (head rel) # xcount
>   --undefined

> time :: IO c -> IO c
> time =
>   bracket getClockTime
>           (\st -> do
>              et <- getClockTime
>              let tdiff = diffClockTimes et st
>              putStrLn $ "time taken: " ++ timeDiffToString tdiff)
>           . const

> assertRelvarValue :: (Database.HaskellDB.Database.GetRec er vr,
>                       Eq vr,
>                       ShowComponents vr) =>
>                      Database
>                   -> Query (Rel (Record er))
>                   -> [Record vr]
>                   -> IO ()
> assertRelvarValue db t v = do
>   r <- query db t
>   assertEqual "" v r

> deRec :: Record t -> t
> deRec (Record a) = a

> querySingleValue :: (GetRec er vr,
>                      ShowComponents vr,
>                      HNat2Integral n,
>                      HLength vr n,
>                      HLookupByHNat HZero vr (LVPair t b)) =>
>                     Database -> Query (Rel (Record er)) -> IO b
> querySingleValue db q =
>   query db q >>= return . getSingleValue

> getSingleValue :: (ShowComponents t1,
>                    HNat2Integral n,
>                    HLength t1 n,
>                    HLookupByHNat HZero t1 (LVPair t a)) =>
>                   [Record t1] -> a
> getSingleValue r =
>     maybe (error $ "expect 1 field, got none: " ++ show r)
>           id $ getMaybeSingleValue r

> getMaybeSingleValue :: (HNat2Integral n,
>                         HLength t n,
>                         ShowComponents t,
>                         HLookupByHNat HZero t (LVPair t1 b)) =>
>                        [Record t] -> Maybe b
> getMaybeSingleValue r =
>     let t' = getMaybeSingleTuple r
>     in flip fmap t' $ \t -> case (hNat2Integral $ hLength $ deRec t)::Int of
>          0 -> error $ "no fields: " ++ show r
>          1 -> lVPairV $ hLookupByHNat hZero $ deRec t
>          n -> error $ "expected 1 field, got " ++ show n ++ " - " ++ show t
>     where
>       lVPairV (LVPair v) = v

> getSingleTuple :: (Show b) => [b] -> b
> getSingleTuple r =
>   maybe (error $ "expected onne tuple, got " ++ show r)
>         id $ getMaybeSingleTuple r

> getMaybeSingleTuple :: (Show b) => [b] -> Maybe b
> getMaybeSingleTuple r = case r of
>                      o : [] -> Just o
>                      [] -> Nothing
>                      _ ->  error $ "expected 0 or 1 tuple, got " ++ show r

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

> mn :: Maybe Int -> Int
> mn = maybe 0 id

> mb :: Maybe Bool -> Bool
> mb = maybe False id
