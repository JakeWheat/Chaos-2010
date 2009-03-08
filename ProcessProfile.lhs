#!/usr/bin/env runghc

> import System.IO
> import System.Environment
> import Text.ParserCombinators.Parsec
> import qualified Text.ParserCombinators.Parsec.Token as P
> import Text.ParserCombinators.Parsec.Language
> import Control.Monad.Identity
> import qualified Text.Parsec.Prim
> import Data.List
> import Graphics.Rendering.Diagrams

> data LineTag = Start | End | Point
>     deriving Show

> data LogLine = LogLine {
>      threadID :: Integer
>     ,logPath :: [String]
>     ,millis :: Integer
>     ,lineTag :: LineTag
>     ,additional :: String
>     } deriving Show

> main :: IO ()
> main = do
>   args <- getArgs
>   logText <- readFile (head args)
>   let logs = case parseLog logText of
>              Left er -> error $ show er
>              Right l -> l
>       times = map millis logs
>       startTime = minimum times
>       fixedLogs = map (\l -> l {millis = (millis l) - startTime}) logs
>   renderAs PNG "profiling.png" Auto $ positionA left top $
>     map (\ll ->
>           ((0, 10 * (fromInteger $ millis ll)), text 15 $ niceLabel ll)
>         ) fixedLogs

> niceLabel :: LogLine -> [Char]
> niceLabel ll = (show $ millis ll) ++ ": " ++
>                  (intercalate "." (logPath ll)) ++
>                  " " ++ additional ll

> data DiagramEntry = DiagramEntry Integer String


> lineToDE :: LogLine -> DiagramEntry
> lineToDE ll = DiagramEntry (millis ll)
>                            (intercalate "." (logPath ll))

> toMillis :: Integer -> Integer -> Integer
> toMillis s ps = (s * ((10::Integer) ^ (3::Integer))) +
>                 (ps `div` ((10::Integer) ^ (9::Integer)))


================================================================================

= log parsing

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
>   return $ LogLine tid lp (toMillis se ps) tag' padd

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
