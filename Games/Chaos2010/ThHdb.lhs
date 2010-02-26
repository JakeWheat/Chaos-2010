

> {-# LANGUAGE TemplateHaskell
>             ,FlexibleInstances
>             ,EmptyDataDecls
>             ,DeriveDataTypeable
>             ,TypeSynonymInstances #-}

-- robbed from the darcs version of hlist

> module Games.Chaos2010.ThHdb (makeLabels,makeValueTypes, makeRecord) where

> --import qualified Language.Haskell.Exts as Exts
> import Database.HaskellDB
> import Language.Haskell.TH.Syntax
> import Data.Char
> import Language.Haskell.TH
> --import Language.Haskell.TH.Ppr
> --import Debug.Trace

> --import Data.Typeable ()
> import Data.Generics.Uniplate.Data

> capitalize, uncapitalize :: String -> String
> capitalize   (c:rest) = toUpper c : rest
> capitalize [] = []
> uncapitalize (c:rest) = toLower c : rest
> uncapitalize [] = []


> dcl_template :: Q [Dec]
> dcl_template = [d| data FooTag --deriving Typeable
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
> makeRecord lvs = [t| Record $(sequence ps >>= foldIt) |]
>     where
>       foldIt :: [Type] -> Q Type
>       foldIt [] = [t|HNil|]
>       foldIt (t:ts) = [t| HCons $(return t) $(foldIt ts)|]
>       ps = map mkPair lvs
>       mkPair (l,v) = [t| LVPair $(conT $ mkName l) $(conT $ mkName v) |]

 > type PieceDescription =
 >     Record (HCons (LVPair Ptype String)
 >             (HCons (LVPair Allegiance String)
 >               (HCons (LVPair Imaginary Bool)
 >                (HCons (LVPair Undead Bool) HNil))))



[TySynD Activate_spells []
   (AppT (ConT Data.HList.Record.Record)
      (AppT
         (AppT (ConT Data.HList.HListPrelude.HCons)
            (AppT
               (AppT (ConT Data.HList.Record.LVPair)
                  (ConT Games.Chaos2010.ThHdb.Spell_name))
               (AppT (ConT Database.HaskellDB.Query.Expr)
                  (ConT GHC.Base.String))))
         (ConT Data.HList.HListPrelude.HNil)))]

> --testMkLabel = runQ (makeLabels ["Test"]) >>= putStrLn . pprint

> {-ppExpr :: Show s => s -> String
> ppExpr s =
>   case Exts.parseExp (show s) of
>     Exts.ParseOk ast -> Exts.prettyPrint ast
>     x -> error $ show x-}

> {-aaa :: Q [Dec]
> aaa = [d| type Activate_spells =
>             Record (HCons (LVPair Spell_name (Expr String)) HNil) |]


> data Spell_nameTag deriving Typeable
> type Spell_name = Proxy Spell_nameTag
> spell_name :: Spell_name
> spell_name = proxy
> instance ShowLabel Spell_name where showLabel _ = "spell_name"


> type Activate_spells =
>     Record (HCons (LVPair Spell_name (Expr String)) HNil)

type Activate_spells_v =
    Record (HCons (LVPair Spell_name String) HNil)


> testMkVT = runQ (makeValueTypes [[t|Activate_spells|]]) >>= putStrLn . pprint

> txxx = [t|Activate_spells|]-}