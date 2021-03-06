Copyright 2010 Jake Wheat

Test utilities for reading and setting pieces.

> {-# LANGUAGE TemplateHaskell,EmptyDataDecls,TypeSynonymInstances,DeriveDataTypeable#-}
> module Games.Chaos2010.Tests.BoardUtils
>     (PieceDescription
>     ,PieceDescriptionPos
>     ,wizardPiecesList
>     ,wizardNames
>     ,parseBoardDiagram
>     ,parseValidSquares
>     ,Pos
>     ,BoardDiagram
>     ,makePD
>     ,makeFPD
>     ,liftPl
>     ,Imaginary
>     ,Undead
>     ,imaginary
>     ,undead
>     ) where

> import Data.Maybe
> import Database.HaskellDB
> import Data.List

> import Games.Chaos2010.Utils
> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.Database.Fields

================================================================================

= Diagrams

In the testing, we frequently use a square list which is either
* a list of x,y co-ordinates used for testing valid squares for an
  action
* list of x,y co-ordinates each with a list of piece descriptions used
  to describe a board, either to set one up or to check against the
  board in the database

So - use a kind of acsii art layout thingy.

The parseDiagram function converts the layout to a list of key,x,y
triples

> type Diagram = [(Char,Int,Int)]

> parseDiagram :: String -> Diagram
> parseDiagram board =
>     let ls' = lines board
>         ls = tail ls'
>     in case () of
>          _ | null ls' -> error "board diagram empty"
>            | head ls' /= "" ->
>                error $ "first line in diagram should be empty string, got " ++
>                          head ls'
>            | length ls /= 10 -> error $ "diagram should have 10 lines, but has " ++
>                                   show (length ls)
>            | any (\l -> length l /= 15) ls ->
>                        error "all lines after first must be \
>                              \15 chars in diagram"
>            | otherwise ->
>                        let sqs = flip concatMap [0..9]
>                                    (\yp -> flip map [0..14]
>                                           (\xp -> ((ls !! yp) !! xp, xp,yp)))
>                        in filter (\(c,_,_) -> c /= ' ') sqs

================================================================================

== valid squares

Valid squares diagram looks like this:
1X     2      3\
XX             \
               \
               \
4             5\
               \
               \
               \
               \
6      7      8\

The numbers are there to get your bearing - they are in the starting
positions for each wizard and are ignored when parsing.

The Xs mark the valid squares (if one is needed on a wizard position
then you leave the wizard number out of the diagram).

> type Pos = $(makeRecord [("X", "Int")
>                         ,("Y", "Int")])

> parseValidSquares :: String -> [Pos]
> parseValidSquares = mapMaybe conv . parseDiagram
>   where
>     conv (c,xp,yp) =
>         case c of
>                 'X' -> Just $ x .=. xp .*. y .=. yp .*. emptyRecord
>                 _ -> if c `elem` ['1'..'8']
>                        then Nothing
>                        else error $ "invalid char in validsquares: " ++ [c]


================================================================================

= test helpers

== board

The board diagram works like the valid squares, but additional needs a
key to match letters and numbers to piece lists.

An example board with the wizards in their starting positions an a
goblin owned by the first wizard next to him:

"\n\
\1G     2      3\
\               \
\               \
\               \
\4             5\
\               \
\               \
\               \
\               \
\6      7      8"
The key is:
(wizardPiecesList ++
[('G', [PieceDescription "goblin" "Buddha" []])]

wizardPiecesList is a list of the wizard pieces to avoid writing them
out each test since most tests have all eight wizards remaining at the
end


> type PieceDescription = $(makeRecord [("Ptype", "String")
>                                      ,("Allegiance", "String")
>                                      ,("Imaginary", "Bool")
>                                      ,("Undead", "Bool")])

> type PieceDescriptionPos = $(makeRecord [("Ptype", "String")
>                                         ,("Allegiance", "String")
>                                         ,("X", "Int")
>                                         ,("Y", "Int")
>                                         ,("Imaginary", "Bool")
>                                         ,("Undead", "Bool")])


The piece tag holds a bit of extra information about that piece to
shorten the tests and avoid having to read out lots of tables when
e.g. if a creature is imaginary or undead, etc. Will probably rethink
this when the tests are expanded in scope.

> makePD :: String -> String -> PieceDescription
> makePD p a = ptype .=. p
>                        .*. allegiance .=. a
>                        .*. imaginary .=. False
>                        .*. undead .=.False
>                        .*. emptyRecord

> makeFPD :: String -> String -> Bool -> Bool -> PieceDescription
> makeFPD p a i u = ptype .=. p
>                        .*. allegiance .=. a
>                        .*. imaginary .=. i
>                        .*. undead .=. u
>                        .*. emptyRecord


> wizardStuff :: [(Char, Int,Int, PieceDescription)]
> wizardStuff = [('1', 0,0, l "wizard" "Buddha")
>                ,('2', 7,0, l "wizard" "Kong Fuzi")
>                ,('3', 14,0, l "wizard" "Laozi")
>                ,('4', 0,4, l "wizard" "Moshe")
>                ,('5', 14,4, l "wizard" "Muhammad")
>                ,('6', 0,9, l "wizard" "Shiva")
>                ,('7', 7,9, l "wizard" "Yeshua")
>                ,('8', 14,9, l "wizard" "Zarathushthra")]
>               where
>                 l p a = ptype .=. p
>                        .*. allegiance .=. a
>                        .*. imaginary .=. False
>                        .*. undead .=.False
>                        .*. emptyRecord

> wizardPiecesList :: [(Char, [PieceDescription])]
> wizardPiecesList = map (\(a,_,_,c) -> (a,[c])) wizardStuff

> wizardNames :: [String]
> wizardNames = map (\(_, _, _,p) -> p # allegiance) wizardStuff

> liftPl :: [(Char, [(String, String)])] -> [(Char, [PieceDescription])]
> liftPl = map (\(c,l) -> (c, flip map l $ \(p,a) -> pd p a))
>          where
>            pd p a = ptype .=. p
>                     .*. allegiance .=. a
>                     .*. imaginary .=. False
>                     .*. undead .=.False
>                     .*. emptyRecord

These are our type alias for the board

A key entry gives a piece list corresponding to a letter on a board
diagram

> type KeyEntry = (Char, [PieceDescription])

The full board diagram is the diagram string plus the key

> type BoardDiagram = (String, [KeyEntry])

> parseBoardDiagram :: BoardDiagram -> ([String]
>                                      ,[PieceDescriptionPos])
> parseBoardDiagram (diagram, ekey) = (wzs, pds)
>   where
>     pds :: [PieceDescriptionPos]
>     pds = concatMap expandKey keyPositionList
>     wzs = filter (/= "dead") $ nub $ map (# allegiance) pds
>     keyPositionList = parseDiagram diagram
>     expandKey :: (Char,Int,Int) -> [PieceDescriptionPos]
>     expandKey (k,xp,yp) = let ps = safeLookup "board diagram parse" k ekey
>                           in flip map ps $ \p -> ptype .=. (p # ptype)
>                                                  .*. allegiance .=. (p # allegiance)
>                                                  .*. x .=. xp
>                                                  .*. y .=. yp
>                                                  .*. imaginary .=. (p # imaginary)
>                                                  .*. undead .=. (p # undead)
>                                                  .*. emptyRecord
