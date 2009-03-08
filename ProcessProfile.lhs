#!/usr/bin/env runghc

> --import Data.Maybe
> import System.IO
> import System.Environment
> --import Text.Regex.PCRE
> --import Control.Monad
> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import Control.Monad.Identity
> import qualified Text.Parsec.Prim


 > import Text.Parsec.Token

> data LineTag = Start | End | Point
>     deriving Show

> data LogLine = LogLine {
>      threadID :: Integer
>     ,logPath :: [String]
>     ,secs :: Integer
>     ,picosecs :: Integer
>     ,lineTag :: LineTag
>     ,additional :: String
>     } deriving Show

> main :: IO ()
> main = do
>   args <- getArgs
>   text <- readFile (head args)
>   putStrLn $ show $ parseLog text

> parseLog :: String -> Either ParseError [LogLine]
> parseLog input = parse logFile "(unknown)" input

> logFile :: GenParser Char st [LogLine]
> logFile = do
>   result <- many line
>   eof
>   return result

> line :: GenParser Char st LogLine
> line = do
>   char '['
>   lp <- lpath
>   char ']'
>   whitespace
>   se <- integer
>   char ':'
>   ps <- integer
>   whitespace
>   string "ThreadId"
>   whitespace
>   tid <- integer
>   whitespace
>   tag <- string "start" <|> string "end"
>   let tag' = case tag of
>                       "start" -> Start
>                       "end" -> End
>                       _ -> error $ "expected start or end, got " ++ tag
>   whitespace

parse until we hit \n[, consuming the \n but not the [

>   padd  <- manyTill
>              anyChar (
>                (try (char '\n' >>
>                      lookAhead (char '['))
>                 >> return ()) <|> eof)
>   return $ LogLine tid lp se ps tag' padd

[chaos.MyTextView.render/DEBUG] 1236534159:94889000000 ThreadId 304 start

> lpath :: GenParser Char st [String]
> lpath = sepBy lpathEle (char '.')

> lpathEle :: GenParser Char st String
> lpathEle = many (noneOf ".]")

> lexer :: P.GenTokenParser String u Identity

> lexer = P.makeTokenParser haskellDef

> integer :: Text.Parsec.Prim.ParsecT String u Identity Integer
> integer = P.integer lexer

> whitespace :: Text.Parsec.Prim.ParsecT String u Identity ()
> whitespace= P.whiteSpace lexer