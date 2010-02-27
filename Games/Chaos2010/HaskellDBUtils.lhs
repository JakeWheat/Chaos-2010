> {-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, FlexibleContexts,TypeSynonymInstances, TemplateHaskell #-}
> module Games.Chaos2010.HaskellDBUtils where

> --import System.Time
> import Database.HaskellDB
> import Database.HaskellDB.Query
> import Database.HaskellDB.PrimQuery
> --import Control.Exception
> import Database.HaskellDB.Database
> import Test.HUnit
> import Data.List ((\\),intercalate)

> import Games.Chaos2010.ThHdb

> $(makeLabels ["xcount"])

> getCount :: Database -> Query t -> IO Int
> getCount db t = do
>   rel <- query db $ do
>                   _ <- t
>                   project $ xcount .=. count (constant (1::Int))
>                             .*. emptyRecord
>   return $ getSingleValue rel
>   --return $ (head rel) # xcount
>   --undefined

> setRelvarT :: (RecordLabels er ls,
>               HLabelSet ls,
>               HRearrange ls r r',
>               RecordValues r' vs',
>               HMapOut
>               ToPrimExprsOp vs' Database.HaskellDB.PrimQuery.PrimExpr,
>               InsertRec r' er,
>               HMap ConstantRecordOp r1 r) =>
>              Database -> Table (Record er) -> Record r1 -> IO ()
> setRelvarT db t v = setRelvar db t [v]

> setRelvar :: (RecordLabels er ls,
>               HLabelSet ls,
>               HRearrange ls r r',
>               RecordValues r' vs',
>               HMapOut ToPrimExprsOp vs' PrimExpr,
>               InsertRec r' er,
>               HMap ConstantRecordOp r1 r) =>
>              Database -> Table (Record er) -> [Record r1] -> IO ()
> setRelvar db t v = do
>   clearTable db t
>   forM_ v $ insert db t . constantRecord


> clearTable :: Database -> Table r -> IO ()
> clearTable db t =
>    delete db t (const $ constant True)


> assertRelvarValue :: (Database.HaskellDB.Database.GetRec er vr,
>                       Eq vr,
>                       ShowComponents vr) =>
>                      Database
>                   -> Query (Rel (Record er))
>                   -> [Record vr]
>                   -> IO ()
> assertRelvarValue db t v = do
>   r <- query db t
>   let b = recsEq v r
>   if not b
>     then putStrLn $ "== missing\n" ++ sh (v \\ r) ++ "\n== extra\n" ++ sh (r \\ v)
>     else return ()
>   assertBool "" b
>   where
>     sh rel = intercalate "\n" $ map show rel

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

> recsEq :: Eq a => [Record a] -> [Record a] -> Bool
> recsEq a b = (null (a \\ b))
>              && null (b \\ a)


> qdb :: (GetRec er vr) =>
>        Database -> Query (Rel (Record er)) -> (Record vr -> b) -> IO [b]
> qdb db t r = query db t >>= return . map r
>
> jn :: Expr (Maybe String) -> Expr String
> jn = fromNull (constant "")

> mv :: Maybe String -> String
> mv = maybe "" id

> smn :: Maybe Int -> String
> smn = show . maybe 0 id

> mn :: Maybe Int -> Int
> mn = maybe 0 id

> mb :: Maybe Bool -> Bool
> mb = maybe False id

> fn :: (ShowConstant a) => a -> Expr (Maybe a) -> Expr a
> fn x = fromNull (constant x)