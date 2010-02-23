Copyright 2010 Jake Wheat

Test utilities for reading and setting pieces.

> module Games.Chaos2010.Tests.BoardUtils
>     (BoardDiagram
>     ,toBoardDescription
>     ,PieceDescription(..)
>     ,wizardNames
>     ,Imaginary(..)
>     ,Undead(..)
>     ,Ptype
>     ,Allegiance
>     ,assertValidSquaresEquals
>     ,assertBoardEquals
>     ,wizardPiecesList
>     ,makePD
>     ,liftPl
>     ,assertTopPiecesEquals
>     ) where

> import Data.Maybe
> import Database.HaskellDB
> import Data.List
> import Test.HUnit


> import qualified Games.Chaos2010.Database.Current_wizard_spell_squares as Cwss
> import qualified Games.Chaos2010.Database.Piece_details as Pd
> import qualified Games.Chaos2010.Database.Pieces_on_top_view as Ptv
> import Games.Chaos2010.Utils

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

> type ValidSquares = [(Int,Int)]

> toValidSquares :: Diagram -> ValidSquares
> toValidSquares =
>     mapMaybe (\(c,xp,yp) ->
>               case c of
>                 'X' -> Just (xp,yp)
>                 _ -> if c `elem` ['1'..'8']
>                        then Nothing
>                        else error $ "invalid char in validsquares: " ++ [c])

corresponding function to read the list of valid squares from the
database hard coded to casting for now, will need to cover other stuff
as well.

> queryValidSquares :: Database -> IO ValidSquares
> queryValidSquares db = do
>   res <- query db $ do
>            tb <- table Cwss.current_wizard_spell_squares
>            project $ copy Cwss.x tb
>                       .*. copy Cwss.y tb
>                       .*. emptyRecord
>   return $ map (\t -> (mn $ t # Cwss.x, mn $ t # Cwss.y)) res

================================================================================

= test helpers

== valid squares

take a valid squares string and check it matches the database
it has to be the cast phase for this to succeed.

> assertValidSquaresEquals :: Database -> String -> IO ()
> assertValidSquaresEquals db vss = do
>     --get our piece tuple lists for the board in the database
>     --and the expected board
>     currentValidSquares <- queryValidSquares db
>     let avs' = sort currentValidSquares
>         evs' = sort $ (toValidSquares . parseDiagram) vss
>     assertEqual "valid squares wrong" evs' avs'

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

> type Allegiance = String
> type Ptype = String

> data PieceDescription =
>     PieceDescription Ptype Allegiance Imaginary Undead
>     deriving (Show,Eq,Ord)

> data Imaginary = Imaginary | Real
>                  deriving (Show,Eq,Ord)
> data Undead = Undead | Alive
>               deriving (Show,Eq,Ord)

 > data PieceTag = PImaginary | PUndead
 >     deriving (Show,Eq,Ord)

The piece tag holds a bit of extra information about that piece to
shorten the tests and avoid having to read out lots of tables when
e.g. if a creature is imaginary or undead, etc. Will probably rethink
this when the tests are expanded in scope.

> makePD :: Ptype -> Allegiance -> PieceDescription
> makePD ptype allegiance = PieceDescription ptype allegiance Real Alive

> wizardStuff :: [(Char, Int,Int, PieceDescription)]
> wizardStuff =  [('1', 0,0, makePD "wizard" "Buddha")
>                ,('2', 7,0, makePD "wizard" "Kong Fuzi")
>                ,('3', 14,0, makePD "wizard" "Laozi")
>                ,('4', 0,4, makePD "wizard" "Moshe")
>                ,('5', 14,4, makePD "wizard" "Muhammad")
>                ,('6', 0,9, makePD "wizard" "Shiva")
>                ,('7', 7,9, makePD "wizard" "Yeshua")
>                ,('8', 14,9, makePD "wizard" "Zarathushthra")]

> wizardPiecesList :: [(Char, [PieceDescription])]
> wizardPiecesList = map (\(a,_,_,c) -> (a,[c])) wizardStuff

> wizardNames :: [String]
> wizardNames = map (\(_, _, _,PieceDescription _ w _ _) -> w) wizardStuff

> liftPl :: [(Char, [(Ptype, Allegiance)])] -> [(Char, [PieceDescription])]
> liftPl = map (\(c,l) -> (c, flip map l $ \(p,a) -> PieceDescription p a Real Alive))

These are our type alias for the board

A key entry gives a piece list corresponding to a letter on a board
diagram

> type KeyEntry = (Char, [PieceDescription])

The full board diagram is the diagram string plus the key

> type BoardDiagram = (String, [KeyEntry])

A board description is what we read out of the database and parse the
board diagram to for comparison and loading into the database.

> type BoardDescription = [(PieceDescription,Int,Int)]

Top level fn, take the expected board description in the form of a
board string and a key, and check it against what is in the database:

> assertBoardEquals :: Database -> BoardDiagram -> IO ()
> assertBoardEquals db bd = do
>   actualBoard <- queryBoard db
>   let expectedBoard = toBoardDescription bd
>   assertEqual "board pieces" (sort expectedBoard) (sort actualBoard)

Like with check board, but we are only want to check the topmost piece
on each square

> assertTopPiecesEquals :: Database -> BoardDiagram -> IO ()
> assertTopPiecesEquals db bd = do
>   actualBoard <- queryPiecesOnTop db
>   let expectedBoard = toBoardDescription bd
>   assertEqual "board pieces on top" (sort expectedBoard) (sort actualBoard)


> toBoardDescription :: BoardDiagram -> BoardDescription
> toBoardDescription (diagram, key) =
>   let keyPositionList = parseDiagram diagram
>   in flip concatMap keyPositionList
>               (\(k,x,y) ->
>                    let ps = safeLookup "board diagram parse" k key
>                    in map (\p -> (p,x,y)) ps)

now the code to read the board from the database and get it in the same format:

> queryBoard :: Database -> IO BoardDescription
> queryBoard db = do
>   rel <- query db $ do
>            tb <- table Pd.piece_details
>            project $ copy Pd.ptype tb
>                        .*. copy Pd.allegiance tb
>                        .*. copy Pd.x tb
>                        .*. copy Pd.y tb
>                        .*. copy Pd.undead tb
>                        .*. copy Pd.imaginary tb
>                        .*. emptyRecord
>   return $ map conv rel
>   where
>     conv t = (PieceDescription (mv $ t # Pd.ptype)
>                                (mv $ t # Pd.allegiance)
>                                (if mb (t # Pd.imaginary) then Imaginary else Real)
>                                (if mb (t # Pd.undead) then Undead else Alive)
>              ,mn $ t # Pd.x
>              ,mn $ t # Pd.y)

The variant for only the topmost pieces:

> queryPiecesOnTop :: Database -> IO BoardDescription
> queryPiecesOnTop db = do
>   rel <- query db $ do
>            tb <- table Ptv.pieces_on_top_view
>            project $ copy Ptv.ptype tb
>                        .*. copy Ptv.allegiance tb
>                        .*. copy Ptv.x tb
>                        .*. copy Ptv.y tb
>                        .*. copy Ptv.undead tb
>                        .*. copy Ptv.imaginary tb
>                        .*. emptyRecord
>   return $ map conv rel
>   where
>     conv t = (PieceDescription (mv $ t # Ptv.ptype)
>                                (mv $ t # Ptv.allegiance)
>                                (if mb (t # Ptv.imaginary) then Imaginary else Real)
>                                (if mb (t # Ptv.undead) then Undead else Alive)
>              ,mn $ t # Ptv.x
>              ,mn $ t # Ptv.y)
