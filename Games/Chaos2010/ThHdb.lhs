

> {-# LANGUAGE TemplateHaskell
>             ,FlexibleInstances
>             ,EmptyDataDecls
>             ,DeriveDataTypeable
>             ,TypeSynonymInstances #-}

-- robbed from the darcs version of hlist

> module Games.Chaos2010.ThHdb (makeLabels) where

> import Database.HaskellDB

> import Language.Haskell.TH.Syntax

> import Data.Char (toUpper, toLower)

> import Data.Typeable ()
> import Data.Generics.Uniplate.Data

> capitalize, uncapitalize :: String -> String
> capitalize   (c:rest) = toUpper c : rest
> capitalize [] = []
> uncapitalize (c:rest) = toLower c : rest
> uncapitalize [] = []


> dcl_template :: Q [Dec]
> dcl_template = [d| data FooTag deriving Typeable
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

> --testMkLabel = runQ (makeLabels ["Test"]) >>= putStrLn . pprint
