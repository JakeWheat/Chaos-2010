> module Games.Chaos2010.Tests.TestUtils
>     (tctor
>
>     ,startNewGame
>     ,newGameReadyToCast
>     ,setupBoard
>     ,startNewGameReadyToAuto
>     ,startNewGameReadyToMove
>     ,startNewGameReadyToMoveNoSpread
>     ,newGameWithBoardReadyToCast
>     ,addSpell
>     ,keyChooseSpell
>     ,killWizard
>     ,addMagicSword
>     ,killTopPieceAt
>
>     ,rigActionSuccess
>     ,sendKeyPress
>     ,goSquare
>     ,skipToPhase
>     ,setCursorPos
>
>     ,queryTurnPhase
>
>     ,assertCurrentWizardPhase
>     ,assertSelectedPiece
>     ,assertMoveSubphase
>     ,assertPieceDoneSelection
>     ,assertNoSelectedPiece
>
>     ,time
>     ) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Monad
> import Control.Exception
> import Data.List
> import System.Time

> import Database.HDBC (IConnection)
> import Database.HaskellDB

> import Games.Chaos2010.DBUpdates hiding (rigActionSuccess)
> import qualified Games.Chaos2010.DBUpdates as DBu
> import Games.Chaos2010.Tests.BoardUtils
> import qualified Games.Chaos2010.Database.Selected_piece as Sp
> import qualified Games.Chaos2010.Database.Pieces_to_move as Ptm
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Current_wizard_table

> tctor :: IConnection conn => String
>       -> (conn -> IO ())
>       -> conn
>       -> Test.Framework.Test
> tctor l f conn = testCase l $ rollbackOnError conn $ time $ f conn

 > type SqlRow = M.Map String String

> newGameReadyToCast :: IConnection conn => Database -> conn -> String -> IO ()
> newGameReadyToCast db conn spellName = do
>   startNewGame conn
>   addSpell conn "Buddha" spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase db conn "cast"

> newGameWithBoardReadyToCast :: IConnection conn =>
>                                Database
>                             -> conn
>                             -> String
>                             -> BoardDiagram
>                             -> IO ()
> newGameWithBoardReadyToCast db conn spellName board = do
>   startNewGame conn
>   setupBoard db conn board
>   addSpell conn "Buddha" spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase db conn "cast"

 > selectInt :: IConnection conn => conn -> String -> [String] -> IO Int
 > selectInt conn q a = undefined {-do
 >     x <- selectValue conn q a
 >     return (read x ::Int)-}

 > assertRelvarValue :: (Read a,Eq a,Show a,IConnection conn) =>
 >                      String -> conn -> String -> [String] -> a -> IO ()
 > assertRelvarValue m conn q args val = undefined {-do
 >   v <- selectValue conn q args
 >   assertEqual m val (read v)-}

== check new game relvars

run after each call to new game to check - hopefully should catch any
problems when all the tests are run since each one runs new game with
the board in the state the last test left it in

todo: add all relvars which aren't readonly to this

 > checkNewGameRelvars :: IConnection conn => conn -> IO ()
 > checkNewGameRelvars conn = do
 >   checkRelvar conn "turn_number_table" ["0"]
 >   checkRelvar conn "current_wizard_table" ["Buddha"]
 >   checkRelvar conn "turn_phase_table" ["choose"]
 >   checkRelvar conn "spell_choice_hack_table" ["False"]
 > --todo: action_history
 >   mapM_ (\x -> checkRelvar conn x []) ["wizard_spell_choices_mr"
 >                                       ,"spell_parts_to_cast_table"
 >                                       ,"cast_success_checked_table"
 >                                       ,"cast_alignment_table"
 >                                       ,"pieces_to_move"
 >                                       ,"selected_piece"
 >                                       ,"remaining_walk_table"]

> goSquare :: IConnection conn => conn -> Int -> Int -> IO ()
> goSquare conn x y = do
>   setCursorPos conn x y
>   sendKeyPress conn "Return"

== some helper functions

something hacked up to check the contents of a relvar in the
database. Should create a better format for writing relvar literals in
this test file.

 > checkRelvar :: IConnection conn => conn -> String -> [String] -> IO ()
 > checkRelvar conn relvarName value = undefined
 >   {-selectTuplesC conn ("select * from " ++ relvarName) []
 >                      (\r -> head $ map snd $ M.toList r) >>=
 >     assertEqual ("relvar " ++ relvarName) value-}

shorthands to check data in the database

 > checkSelectedPiece :: Database -> [Char] -> [Char] -> IO ()
 > checkSelectedPiece db allegiance ptype = do
 >   rel <- query db $ table Sp.selected_piece
 >   let t = head rel
 >   assertEqual "selected piece" (allegiance,ptype)
 >               (t # Sp.allegiance, t # Sp.ptype)

> assertMoveSubphase :: Database -> [Char] -> IO ()
> assertMoveSubphase db sp = do
>   rel <- query db $ do
>            t1 <- table Sp.selected_piece
>            project $ copy Sp.move_phase t1
>                      .*. emptyRecord
>   let t = head rel
>   assertEqual "move subphase" sp (t # Sp.move_phase)

> assertNoSelectedPiece :: Database -> IO ()
> assertNoSelectedPiece db = do
>   rel <- query db $ table Sp.selected_piece
>   assertBool ("there should be no selected piece, got " ++
>               let r = head rel
>               in r # Sp.ptype ++ " - " ++ r # Sp.allegiance ++
>                  " " ++ r # Sp.move_phase)
>              (null rel)

================================================================================

= setup game functions

== setup board

this takes a board description and sets the board to match it used to
setup a particular game before running some tests

> setupBoard :: IConnection conn => Database -> conn -> BoardDiagram -> IO ()
> setupBoard _ conn bd = do
>   let targetBoard = toBoardDescription bd
>   --just assume that present wizards are in the usual starting positions
>   --fix this when needed
>   let (wizardItems, nonWizardItems) =
>          partition (\(PieceDescription t _ _ _, _, _) -> t == "wizard")
>          targetBoard
>       isWizPresent name = any (\(PieceDescription _ n _ _, _, _) ->
>                                n == name) wizardItems
>   -- remove missing wizards
>   forM_ [0..7] (\i ->
>     unless (isWizPresent $ wizardNames !! i) $ killWizard conn (wizardNames !! i))
>   -- move present wizards
>   forM_ wizardItems (\(PieceDescription _ name _ _, x, y) -> do
>     {-t <- selectTuple conn "select x,y from pieces where ptype='wizard'\n\
>                      \and allegiance=?" [name]
>     unless (read (lk "x" t) == x && read (lk "y" t) == y)-}
>            setWizardPosition conn name x y)
>   -- add extra pieces
>   forM_ nonWizardItems $ \(PieceDescription ptype allegiance im un, x, y) -> do
>     if allegiance == "dead"
>       then createCorpse conn ptype x y (im == Imaginary)
>       else createPieceInternal conn ptype allegiance x y (im == Imaginary) (un == Undead)
>   return ()

this overrides the next random test by name e.g. so we can test the
board after a spell has succeeded and after it has failed

> rigActionSuccess :: IConnection conn => conn -> String -> Bool -> IO ()
> rigActionSuccess conn override setting = do
>   DBu.rigActionSuccess conn override setting
>   resetCurrentEffects conn

================================================================================

= database update helpers

> {-startNewGame :: IConnection conn => conn -> IO ()
> startNewGame conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='human'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}

> {-startNewGameAI :: IConnection conn => conn -> IO ()
> startNewGameAI conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='computer'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}


Adds the spell given to the first wizard's spell book


keep running next_phase until we get to the cast phase

> skipToPhase :: IConnection conn => Database -> conn -> [Char] -> IO ()
> skipToPhase db conn phase = do
>   unless (phase `elem` ["choose","cast","move"])
>          (error $ "unrecognised phase: " ++ phase)
>   whenA1 (queryTurnPhase db)
>          (/= phase) $ do
>          sendKeyPress conn "space"
>          skipToPhase db conn phase

> rollbackOnError :: IConnection conn => conn -> IO c -> IO c
> rollbackOnError conn =
>     bracketOnError (return())
>                    (const $ rollback conn) . const

> startNewGameReadyToMove :: IConnection conn => Database -> conn -> BoardDiagram -> IO ()
> startNewGameReadyToMove db conn board = do
>   startNewGame conn
>   setupBoard db conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase db conn "move"

> startNewGameReadyToMoveNoSpread :: IConnection conn => Database -> conn -> BoardDiagram -> IO ()
> startNewGameReadyToMoveNoSpread db conn board = do
>   startNewGame conn
>   disableSpreading conn
>   setupBoard db conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase db conn "move"

> startNewGameReadyToAuto :: IConnection conn => Database -> conn -> BoardDiagram -> IO ()
> startNewGameReadyToAuto db conn board = do
>   startNewGame conn
>   setupBoard db conn board
>   sendKeyPress conn $ keyChooseSpell "disbelieve"
>   skipToPhase db conn "cast"


================================================================================

= database read shortcuts

> queryTurnPhase :: Database -> IO String
> queryTurnPhase db = do
>   rel <- query db $ table turn_phase_table
>   let t = head rel
>   return (t # turn_phase)

> queryCurrentWizard :: Database -> IO String
> queryCurrentWizard db = do
>   rel <- query db $ table current_wizard_table
>   let t = head rel
>   return (t # current_wizard)

> assertSelectedPiece :: Database -> Ptype -> Allegiance -> IO()
> assertSelectedPiece db ptype allegiance = do
>   rel <- query db $ table Sp.selected_piece
>   let t = head rel
>   assertEqual "selected piece" (ptype,allegiance)
>               (t # Sp.ptype, t # Sp.allegiance)

> assertCurrentWizardPhase :: Database -> String -> String -> IO()
> assertCurrentWizardPhase db wiz phase = do
>   wiz' <- queryCurrentWizard db
>   phase' <- queryTurnPhase db
>   assertEqual "current wizard" wiz wiz'
>   assertEqual "current phase" phase' phase

> assertPieceDoneSelection :: Database -> String -> String -> IO ()
> assertPieceDoneSelection db ptype allegiance = do
>     assertNoSelectedPiece db
>     rel <- query db $ do
>              t1 <- table Ptm.pieces_to_move
>              restrict ((t1 .!. Ptm.ptype) .==. constant ptype)
>              restrict ((t1 .!. Ptm.allegiance) .==. constant allegiance)
>              project $ copyAll t1
>     assertBool "piece not in ptm" (null rel)

================================================================================

= keyboard stuff

create wrappers around the key press stuff to make the tests easier to
understand

> lookupChooseSpellKeys :: [([Char], [Char])]
> lookupChooseSpellKeys = [("goblin", "m")
>                         ,("disbelieve", "Q")
>                         ,("magic_wood", "G")
>                         ,("shadow_wood", "M")
>                         ,("magic_bolt", "D")
>                         ,("vengeance", "E")
>                         ,("subversion", "R")
>                         ,("raise_dead", "V")
>                         ,("magic_knife", "1")
>                         ,("magic_shield", "2")
>                         ,("magic_armour", "3")
>                         ,("magic_bow", "4")
>                         ,("magic_sword", "5")
>                         ,("shadow_form", "6")
>                         ,("magic_wings", "7")
>                         ,("law", "O")
>                         ]


> keyChooseSpell :: String -> String
> keyChooseSpell spellName = safeLookup
>                              "get key for spell" spellName
>                              lookupChooseSpellKeys


===============================================================================

> startNewGame :: IConnection conn => conn -> IO ()
> startNewGame conn = do
>   resetNewGameWidgetState conn
>   setAllHuman conn
>   newGame conn
>   resetCurrentEffects conn

> whenA1 :: IO a -> (a -> Bool) -> IO () -> IO ()
> whenA1 feed cond f = (cond `liftM` feed) >>= flip when f

> time :: IO c -> IO c
> time =
>   bracket getClockTime
>           (\st -> do
>              et <- getClockTime
>              let tdiff = diffClockTimes et st
>              putStrLn $ "time taken: " ++ timeDiffToString tdiff)
>           . const
