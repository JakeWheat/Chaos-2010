#! /usr/bin/env runhaskell

> import Control.Applicative
> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> import Data.Generics
> import Data.Generics.Uniplate.Data
> import Data.Char
> import Games.Chaos2010.ThHdb
> import Language.Haskell.TH.Syntax
> import Data.Char
> import Language.Haskell.TH
> import Data.List hiding (find)
> import System.FilePath.Find

> main :: IO ()
> main  = do
>   --let files ::[FilePath]
>   --    files = ["Games/Chaos2010/Database/Creature_pieces.hs"]
>   fs <- files
>   fields <- concat <$> mapM processFile fs
>   makeFields fields
>   where
>     files = do
>       find always sourceFileP "Games/Chaos2010/Database/"
>     sourceFileP = extension ==? ".hs"

---------------------------------------------------------------------------

> processFile :: FilePath -> IO [String]
> processFile fn = do
>   ast <- pf fn
>   let fs =  getFields ast
>       ast1 = removeFields (idHack fs) $ addImport ast
>   --putStrLn $ ppExpr $ ast1
>   writeFile fn $ prettyPrint ast1
>   return $ fs
>   where
>     idHack fs = if "xid" `elem` fs
>                 then "id":fs
>                 else fs
>     addImport :: Module -> Module
>     addImport (Module s n o wt es i ds) = Module s n o wt es (imp:i) ds
>     imp = ImportDecl nsrc
>            (ModuleName "Games.Chaos2010.Database.Fields")
>            False False Nothing Nothing Nothing
>     getFields :: Module -> [String]
>     getFields ast = [f | PatBind _
>                          (PVar(Ident f))
>                          Nothing
>                          (UnGuardedRhs (Var (UnQual (Ident "proxy"))))
>                          (BDecls []) <- universeBi ast]
>     removeFields :: [String] -> Module -> Module
>     removeFields fs = removeData fs
>                       . removeType fs
>                       . removeInstance fs
>                       . removeSig fs
>                       . removeProxy fs
>     removeData :: [String] -> Module -> Module
>     removeData fs = let names = map (\f -> capitalize f ++ "Tag") fs
>                     in transformBi $ \x ->
>                         case x of
>                            (DataDecl _ DataType _ (Ident i) _ _ _):t | i `elem` names -> t
>                            x1 -> x1
>     removeType :: [String] -> Module -> Module
>     removeType fs = let names = map (\f -> capitalize f) fs
>                     in transformBi $ \x ->
>                         case x of
>                            (TypeDecl _ (Ident i) _ _):t | i `elem` names -> t
>                            x1 -> x1
>     removeInstance :: [String] -> Module -> Module
>     removeInstance fs = let names = map (\f -> capitalize f) fs
>                         in transformBi $ \x ->
>                             case x of
>                                    (InstDecl _ _
>                                     (UnQual (Ident "ShowLabel"))
>                                     [TyCon (UnQual (Ident i))]
>                                     _):t | i `elem` names -> t
>                                    x1 -> x1
>     removeSig :: [String] -> Module -> Module
>     removeSig fs = transformBi $ \x ->
>                        case x of
>                               (TypeSig _ [Ident i] _):t  | i `elem` fs -> t
>                               x1 -> x1

   TypeSig
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 96, srcColumn = 1})
     [Ident "flying"]
     (TyCon (UnQual (Ident "Flying"))),
   PatBind
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 97, srcColumn = 1})
     (PVar (Ident "flying"))
     Nothing
     (UnGuardedRhs (Var (UnQual (Ident "proxy"))))
     (BDecls []),

>     removeProxy :: [String] -> Module -> Module
>     removeProxy fs = transformBi $ \x ->
>                          case x of
>                               (PatBind _ (PVar (Ident i)) _ _ _):t | i `elem` fs -> t
>                               x1 -> x1


> capitalize, uncapitalize :: String -> String
> capitalize   (c:rest) = toUpper c : rest
> capitalize [] = []
> uncapitalize (c:rest) = toLower c : rest
> uncapitalize [] = []


   DataDecl
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 92, srcColumn = 1})
     DataType
     []
     (Ident "FlyingTag")
     []
     []
     [],
   TypeDecl
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 93, srcColumn = 1})
     (Ident "Flying")
     []
     (TyApp (TyCon (UnQual (Ident "Proxy")))
        (TyCon (UnQual (Ident "FlyingTag")))),
   InstDecl
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 94, srcColumn = 1})
     []
     (UnQual (Ident "ShowLabel"))
     [TyCon (UnQual (Ident "Flying"))]
     [InsDecl
        (FunBind
           [Match
              (SrcLoc{srcFilename =
                        "Games/Chaos2010/Database/Creature_pieces.hs",
                      srcLine = 94, srcColumn = 33})
              (Ident "showLabel")
              [PWildCard]
              Nothing
              (UnGuardedRhs (Lit (String "flying")))
              (BDecls [])])],
   TypeSig
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 96, srcColumn = 1})
     [Ident "flying"]
     (TyCon (UnQual (Ident "Flying"))),
   PatBind
     (SrcLoc{srcFilename =
               "Games/Chaos2010/Database/Creature_pieces.hs",
             srcLine = 97, srcColumn = 1})
     (PVar (Ident "flying"))
     Nothing
     (UnGuardedRhs (Var (UnQual (Ident "proxy"))))
     (BDecls []),


> nsrc :: SrcLoc
> nsrc = SrcLoc "" 0 0


---------------------------------------------------------------------------

> makeFields :: [String] -> IO ()
> makeFields fs = do
>   s <- makeFields
>   let t = replace "Data.HList.Record." "" $ replace "Data.HList.FakePrelude." "" s
>   writeFile "Games/Chaos2010/Database/Fields.hs" $ pre ++ idHack ++ t
>   where
>     makeFields :: IO String
>     makeFields = do
>       x <- runQ ml
>       return $ pprint x
>     ml :: Q [Dec]
>     ml = makeLabels $ filter (`notElem` ["id","xid"]) $ nub fs
>     pre = "{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}\n\
>           \module Games.Chaos2010.Database.Fields where\n\
>           \\n\
>           \import Database.HaskellDB.DBLayout\n\
>           \\n"
>     idHack = "data IdTag\n\
>              \type Id = Proxy IdTag\n\
>              \instance ShowLabel Id where showLabel _ = \"id\"\n\
>              \\n\
>              \xid :: Id\n\
>              \xid = proxy\n"

---------------------------------------------------------------------------

> pf :: String -> IO Module
> pf f = do
>   x <- parseFile f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e

> ppExpr :: Show s => s -> String
> ppExpr s =
>   case Exts.parseExp (show s) of
>     Exts.ParseOk ast -> Exts.prettyPrint ast
>     x -> error $ show x

>
> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'
