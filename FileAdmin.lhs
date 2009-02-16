#! /usr/bin/env runghc

> module FileAdmin (checkSprites, convertToDos, convertToUnix) where

> import System.Directory
> import Utils
> import Data.List
> import Control.Monad
> import System.Environment
> import Data.Char
> import Test.QuickCheck
> import Text.RegexPR
> import qualified Conf as Conf
> import Text.Regex.Posix
> import ChaosDB
> import System.FilePath



function to check all the sprite files are present and all the
pngs under sprites are referenced by the sprite table.
todo: doesn't check all the sprite files are present

> checkSprites :: IO ()
> checkSprites = do
>   conf <- Conf.getConfig
>   spriteFiles <- withConn ("host=localhost dbname=" ++ Conf.dbName conf ++
>                    " user=" ++ Conf.username conf ++
>                    " password=" ++ Conf.password conf)
>     (\conn -> do
>        spriteNames <- selectSingleColumn conn "select sprite from sprites"
>        maybeSpriteFiles <- findAllFiles "sprites"
>        let matchesSprite sn fn = takeFileName fn =~
>                                    ("^" ++ sn ++ "\\.[0-9]\\.png")
>            filesForSprite sn = filter (matchesSprite sn) maybeSpriteFiles
>        return $ concatMap filesForSprite spriteNames)
>   maybeSpriteFiles <- findAllFiles "sprites"
>   let orphans = maybeSpriteFiles \\ spriteFiles
>   if length orphans == 0
>     then putStrLn "OK: no unmatched pngs"
>     else putStrLn $ "unmatched pngs:\n" ++
>            intercalate "\n" orphans

> convertToDos = convertFiles unix2dos
> convertToUnix = convertFiles dos2unix

 > main = do
 >   args <- getArgs
 >   case True of
 >     _ | (length args == 1 && head args == "windows") ->
 >           convertFiles unix2dos
 >     _ | (length args == 1 && head args == "unix") ->
 >           convertFiles dos2unix
 >     _ | otherwise ->
 >             putStrLn "use 'fileAdmin windows' to convert source files \
 >                      \to windows line endings and 'fileAdmin unix' to \
 >                      \convert to unix line endings"

> convertFiles conv = do
>   sf <- getSourceFiles
>   mapM_ (doFile conv) sf
>   mapM_ (\f -> do
>            deleteIfExists f
>            renameFile (f ++ ".new") f) sf

> getSourceFiles :: IO [String]
> getSourceFiles = do
>   x <- getDirectoryContents "."
>   return $ filter isSourceFile x
>   where
>     isSourceFile f = or $ applyMany (map isSuffixOf [".lhs", ".sql"]) f

> doFile conv f = do
>     b <- readFile f
>     let f' = f ++ ".new"
>     writeFile f' (conv b)

> cr = chr 0x0D
> lf = chr 0x0A

> dos2unix :: String -> String
> dos2unix s = gsubRegexPR "\r\n" "\n" s

> unix2dos :: String -> String
> unix2dos s = gsubRegexPR "(?<!\r)\n" "\r\n" s

> countNewlines s = (length $ elemIndices '\n' s)

> prop_duud s = --collect (length $elemIndices '\n' s) $
>                        forAll unixString $ \s ->
>                          (dos2unix $ unix2dos s) == s
> prop_uddu s = --collect (length $elemIndices '\n' s) $
>                        forAll dosString $ \s ->
>                          (unix2dos $ dos2unix s) == s
> prop_udud s = --collect (length $ elemIndices '\n' s) $
>                        forAll unixString $ \s ->
>                          unix2dos s == (unix2dos $ unix2dos s)
> prop_dudu s = --collect (length $ elemIndices '\n' s) $
>                        forAll dosString $ \s ->
>                          dos2unix s == (dos2unix $ dos2unix s)

> instance Arbitrary Char where
>     arbitrary = frequency [(1, return '\n'),
>                            (1, return '\r'),
>                            (1, elements (['\32'..'\127']))]
>     coarbitrary = undefined

> unixString = sized (\n -> replicateM n unixLetter)
> dosString = liftM concat $ sized (\n -> replicateM n dosStringPart)
> dosStringPart = frequency [(1, nonNewlineString),
>                            (1, return "\r\n")]
> nonNewlineString = sized (\n -> replicateM n nonNewlineLetter)
> nonNewlineLetter = elements (['\32'..'\127'])
> unixLetter = frequency [(1, return '\n'),
>                        (1, nonNewlineLetter)]
