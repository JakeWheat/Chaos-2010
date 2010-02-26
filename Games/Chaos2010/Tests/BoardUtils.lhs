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
>     ,BoardDiagram
>     ,BoardDescription
>     {-,toBoardDescription
>     ,PieceDescription(..)
>     ,wizardNames
>     ,assertValidSquaresEquals
>     ,assertBoardEquals
>     ,wizardPiecesList
>     ,makePD-}
>     ,liftPl
>     {-,assertTopPiecesEquals
>     ,newSetupGame
>     ,newGameReadyToCast1-}
>     ,Imaginary
>     ,Undead
>     ,imaginary
>     ,undead
>     ) where

> import Data.Maybe
> import Database.HaskellDB
> import Data.List
> import Test.HUnit
> import Database.HDBC (IConnection)


> --import Games.Chaos2010.Tests.SetupGameState

> import qualified Games.Chaos2010.Database.Current_wizard_spell_squares as Cwss
> import qualified Games.Chaos2010.Database.Piece_details as Pd
> import qualified Games.Chaos2010.Database.Pieces_on_top_view as Ptv
> import Games.Chaos2010.Database.Fields
> --import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Utils
> import Games.Chaos2010.HaskellDBUtils
> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.Tests.TableValueTypes

> parseValidSquares = undefined

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


> $(makeLabels ["Imaginary", "Undead"])

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
 > makePD ptype allegiance = PieceDescription ptype allegiance Real Alive

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

> parseBoardDiagram :: BoardDiagram -> ([PieceDescriptionPos]
>                                      ,[String])

> parseBoardDiagram (diagram, key) = (pds, wzs)
>   where
>     pds :: [PieceDescriptionPos]
>     pds = concatMap expandKey keyPositionList
>     wzs = filter (/= "dead") $ nub $ map (# allegiance) pds
>     keyPositionList = parseDiagram diagram
>     expandKey :: (Char,Int,Int) -> [PieceDescriptionPos]
>     expandKey (k,xp,yp) = let ps = safeLookup "board diagram parse" k key
>                           in flip map ps $ \p -> ptype .=. (p # ptype)
>                                                  .*. allegiance .=. (p # allegiance)
>                                                  .*. x .=. xp
>                                                  .*. y .=. yp
>                                                  .*. imaginary .=. (p # imaginary)
>                                                  .*. undead .=. (p # undead)
>                                                  .*. emptyRecord
>   {-let keyPositionList = parseDiagram diagram
>   in flip concatMap keyPositionList
>               (\(k,x,y) ->
>                    let ps = safeLookup "board diagram parse" k key
>                    in map (\p -> (p,x,y)) ps)
>   where
>     piueces
>     wz = 
>     makeKeys 
>     pd p a i u = ptype .=. p
>                  .*. allegiance .=. a
>                  .*. imaginary .=. i
>                  .*. undead .=. u
>                  .*. emptyRecord-}

A board description is what we read out of the database and parse the
board diagram to for comparison and loading into the database.

> type BoardDescription = [(PieceDescription,Int,Int)]

Top level fn, take the expected board description in the form of a
board string and a key, and check it against what is in the database:

> assertBoardEquals :: Database -> BoardDiagram -> IO ()
> assertBoardEquals db bd = do
>   actualBoard <- queryBoard db
>   let expectedBoard = toBoardDescription bd
>   assertEqual "board pieces" expectedBoard actualBoard

Like with check board, but we are only want to check the topmost piece
on each square

> assertTopPiecesEquals :: Database -> BoardDiagram -> IO ()
> assertTopPiecesEquals db bd = do
>   actualBoard <- queryPiecesOnTop db
>   let expectedBoard = toBoardDescription bd
>   assertEqual "board pieces on top" expectedBoard actualBoard


> toBoardDescription :: BoardDiagram -> BoardDescription
> toBoardDescription (diagram, key) =
>   let keyPositionList = parseDiagram diagram
>   in flip concatMap keyPositionList
>               (\(k,xp,yp) ->
>                    let ps = safeLookup "board diagram parse" k key
>                    in map (\p -> (p,xp,yp)) ps)

> {-type NewBoardDescription = ([Wizards_v]
>                            ,[Pieces_v]
>                            ,[Imaginary_pieces_v]
>                            ,[Crimes_against_nature_v])-}

> {-setFromBoardDiagram :: BoardDiagram -> GameState -> GameState
> setFromBoardDiagram bd gs =
>     let bd1 = toBoardDescription bd
>         items = map (\(((PieceDescription p a i u),xp,yp),t) ->
>                       (p,a,t,i,u,xp,yp)) $ zip bd1 [0..]
>         pieces = flip map items $ \(p,a,t,_,_,xp,yp)
>                                 -> makePiece p a t xp yp
>         im_pieces = flip map (filter isImag items)
>                          $ \(p,a,t,_,_,_,_)
>                              -> makeImag p a t
>         crimes = flip map (filter isCrime items)
>                       $ \(p,a,t,_,_,_,_)
>                           -> makeCrime p a t
>         allegs = catMaybes $ flip map items
>                            $ \(p,a,_,_,_,_,_) ->
>                                if p == "wizard"
>                                then Just a
>                                else Nothing
>         deadWizNos = flip map (restrictTable defaultWizardList
>                                (\r -> r # wizard_name `notElem` allegs))
>                         $ \r -> r # original_place
>     in (gs {peeces = pieces
>           ,imaginaryPieces = im_pieces
>           ,crimesAgainstNature = crimes
>           })
>     where
>       isImag (_,_,_,i,_,_,_) = i == Imaginary
>       isCrime (_,_,_,_,u,_,_) = u== Undead
>       updateTable t w u =
>         flip map t $ \r -> if w r
>                            then u r
>                            else r
>       restrictTable t w = flip filter t $ \r -> w r-}

> {-newSetupGame :: IConnection conn => Database
>              -> conn
>              -> (GameState -> GameState)
>              -> BoardDiagram
>              -> IO ()
> newSetupGame db conn gsm bd = do
>   let (w,p,i,c) = toNewBoardDescription bd
>   setupGame db conn $ gsm $ defaultGameState {wezards = w
>                                              ,peeces = p
>                                              ,imaginaryPieces = i
>                                              ,crimesAgainstNature = c
>                                              }

> newGameReadyToCast1 :: IConnection conn => Database
>                     -> conn
>                     -> (GameState -> GameState)
>                     -> String
>                     -> Maybe Bool
>                     -> BoardDiagram
>                     -> IO ()
> newGameReadyToCast1 db conn gsm sp im bd = do
>   let (w,p,i,c) = toNewBoardDescription bd
>       gs = gsm $ defaultGameState{wezards = w
>                                  ,peeces = p
>                                  ,imaginaryPieces = i
>                                  ,crimesAgainstNature = c
>                                  }
>   newGameReadyToCast db conn sp im gs-}




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
>     conv t = (pd (mv $ t # Pd.ptype)
>                  (mv $ t # Pd.allegiance)
>                  (mb $ t # Pd.imaginary)
>                  (mb $ t # Pd.undead)
>              ,mn $ t # Pd.x
>              ,mn $ t # Pd.y)
>     pd p a i u = ptype .=. p
>                  .*. allegiance .=. a
>                  .*. imaginary .=. i
>                  .*. undead .=. u
>                  .*. emptyRecord

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
>     conv t = (pd (mv $ t # Ptv.ptype)
>                  (mv $ t # Ptv.allegiance)
>                  (mb $ t # Ptv.imaginary)
>                  (mb $ t # Ptv.undead)
>              ,mn $ t # Ptv.x
>              ,mn $ t # Ptv.y)
>     pd p a i u = ptype .=. p
>                  .*. allegiance .=. a
>                  .*. imaginary .=. i
>                  .*. undead .=. u
>                  .*. emptyRecord
