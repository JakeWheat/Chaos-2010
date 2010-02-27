

> {-# LANGUAGE TemplateHaskell
>             ,FlexibleInstances
>             ,EmptyDataDecls
>             ,DeriveDataTypeable
>             ,TypeSynonymInstances #-}

-- some of this was robbed from the darcs version of hlist

> module Games.Chaos2010.ThHdb
>     (makeLabels
>     ,makeValueTypes
>     ,makeRecord
>     ,makeExprRecord) where

> import Database.HaskellDB
> import Language.Haskell.TH.Syntax
> import Data.Char
> import Language.Haskell.TH
> import Data.Generics.Uniplate.Data

> capitalize, uncapitalize :: String -> String
> capitalize   (c:rest) = toUpper c : rest
> capitalize [] = []
> uncapitalize (c:rest) = toLower c : rest
> uncapitalize [] = []


> dcl_template :: Q [Dec]
> dcl_template = [d| data FooTag
>                    type Foo = Proxy FooTag
>                    foo :: Foo
>                    foo = proxy
>                    instance ShowLabel Foo where showLabel _ = "foo" |]

> replace :: String -> [Dec] -> Q [Dec]
> replace i ast =
>   tNames ast >>= tSl
>     where
>         ls = uncapitalize i
>         us = capitalize i
>         ust = us ++ "Tag"
>         tNames = transformBiM $ \x ->
>                    case x of
>                      x1 | x1 == mkName "FooTag" -> return $ mkName ust
>                         | x1 == mkName "Foo" -> return $ mkName us
>                         | x1 == mkName "foo" -> return $ mkName ls
>                      x1 -> return x1
>         tSl = transformBiM $ \x ->
>                    case x of
>                      StringL "foo" -> return $ StringL ls
>                      x1 -> return x1

> makeLabels :: [String] -> Q [Dec]
> makeLabels = liftM concat . sequence . map repl
>   where
>     repl :: String -> Q [Dec]
>     repl n = dcl_template >>= replace n


> makeValueTypes :: [Q Type] -> Q [Dec]
> makeValueTypes =
>   liftM concat . sequence . map makeValueType
>   where
>     makeValueType t = do
>         t1 <- t
>         case t1 of
>                 ConT n -> lac n
>                 _ -> fail "wrong sort of type used in makeValueRecord"
>     lac tn = let i = reify tn
>             in do
>               TyConI (TySynD n ns t) <- i
>               let n1 = nameBase n ++ "_v"
>               t1 <- conv t
>               return [TySynD (mkName n1) ns t1]
>     conv = transformBiM $ \x ->
>                    case x of
>                      (AppT (ConT e) ti) | e == ''Expr -> return ti
>                      x1 -> return x1


> makeRecord :: [(String,String)] -> Q Type
> makeRecord lvs = [t| Record $(sequence (map mkPair lvs) >>= foldIt) |]
>     where
>       mkPair (l,v) = [t| LVPair $(conT $ mkName l) $(conT $ mkName v) |]

> makeExprRecord :: [(String,String)] -> Q Type
> makeExprRecord lvs = [t| Record $(sequence (map mkPair lvs) >>= foldIt) |]
>     where
>       mkPair (l,v) = [t| LVPair $(conT $ mkName l) (Expr $(conT $ mkName v)) |]

> foldIt :: [Type] -> Q Type
> foldIt [] = [t|HNil|]
> foldIt (t:ts) = [t| HCons $(return t) $(foldIt ts)|]
