> module Games.Chaos2010.Tests.TestUtils  where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.List
> import qualified Data.Map as M
> --import qualified Data.Char as DC
> import Control.Monad
> import Data.Maybe
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Exception

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HaskellDB
> import Database.HDBC.PostgreSQL
> import Database.HDBC

> import Games.Chaos2010.Tests.BoardUtils

> tctor :: String
>       -> (Connection -> IO ())
>       -> Connection
>       -> Test.Framework.Test
> tctor l f conn = testCase l $ rollbackOnError conn $ f conn

> type SqlRow = M.Map String String

> newGameReadyToCast :: Connection -> String -> IO ()
> newGameReadyToCast conn spellName = do
>   startNewGame conn
>   addSpell conn spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase conn "cast"

> newGameWithBoardReadyToCast :: Connection
>                             -> String
>                             -> BoardDiagram
>                             -> IO ()
> newGameWithBoardReadyToCast conn spellName board = do
>   startNewGame conn
>   setupBoard conn board
>   addSpell conn spellName
>   sendKeyPress conn  $ keyChooseSpell spellName
>   skipToPhase conn "cast"

> selectInt :: Connection -> String -> [String] -> IO Int
> selectInt conn q a = undefined {-do
>     x <- selectValue conn q a
>     return (read x ::Int)-}

> assertRelvarValue :: (Read a,Eq a,Show a) =>
>                      String -> Connection -> String -> [String] -> a -> IO ()
> assertRelvarValue m conn q args val = undefined {-do
>   v <- selectValue conn q args
>   assertEqual m val (read v)-}

== check new game relvars

run after each call to new game to check - hopefully should catch any
problems when all the tests are run since each one runs new game with
the board in the state the last test left it in

todo: add all relvars which aren't readonly to this

> checkNewGameRelvars :: Connection -> IO ()
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

> goSquare :: Connection -> Int -> Int -> IO ()
> goSquare conn x y = do
>   undefined -- moveCursorTo conn x y
>   sendKeyPress conn "Return"

== some helper functions

something hacked up to check the contents of a relvar in the
database. Should create a better format for writing relvar literals in
this test file.

> checkRelvar :: Connection -> String -> [String] -> IO ()
> checkRelvar conn relvarName value = undefined
>   {-selectTuplesC conn ("select * from " ++ relvarName) []
>                      (\r -> head $ map snd $ M.toList r) >>=
>     assertEqual ("relvar " ++ relvarName) value-}

shorthands to check data in the database

> checkSelectedPiece :: Connection -> [Char] -> [Char] -> IO ()
> checkSelectedPiece conn allegiance ptype = undefined {-do
>   v <- selectTuple conn "select * from selected_piece"
>   assertEqual "selected piece" (allegiance,ptype)
>               (lk "allegiance" v, lk "ptype" v)-}

> checkMoveSubphase :: Connection -> [Char] -> IO ()
> checkMoveSubphase conn sp = undefined {-do
>   ms <- selectTuple conn "select move_phase from selected_piece"
>   assertEqual "move subphase" sp (lk "move_phase" ms)-}

> checkNoSelectedPiece :: Connection -> IO ()
> checkNoSelectedPiece conn = undefined {-do
>   v <- selectTupleIf conn "select * from selected_piece"
>   assertBool ("there should be no selected piece, got " ++
>               let r =  fromJust v
>               in lk "ptype" r ++ " - " ++ lk "allegiance" r ++
>                  " " ++ lk "move_phase" r)
>              (isNothing v)-}

================================================================================

= setup game functions

== setup board

this takes a board description and sets the board to match it used to
setup a particular game before running some tests

> setupBoard :: Connection -> BoardDiagram -> IO ()
> setupBoard conn bd = undefined {-do
>   let targetBoard = parseBoardDiagram bd
>   --just assume that present wizards are in the usual starting positions
>   --fix this when needed
>   let (wizardItems, nonWizardItems) =
>          partition (\(makePD t _ _, _, _) -> t == "wizard")
>          targetBoard
>       isWizPresent name = any (\(makePD _ n _, _, _) ->
>                                n == name) wizardItems
>   -- remove missing wizards
>   forM_ [0..7] (\i ->
>     unless (isWizPresent $ wizardNames !! i) $
>       callSp conn "kill_wizard" [wizardNames !! i])
>   -- move present wizards
>   forM_ wizardItems (\(makePD _ name _, x, y) -> do
>     t <- selectTuple conn "select x,y from pieces where ptype='wizard'\n\
>                      \and allegiance=?" [name]
>     unless (read (lk "x" t) == x && read (lk "y" t) == y)
>            (runSql conn "update pieces set x = ?, y = ?\n\
>                        \where ptype='wizard' and allegiance=?"
>                    [show x, show y, name]))
>   -- add extra pieces
>   forM_ nonWizardItems $ \(makePD ptype allegiance tags, x, y) -> do
>     if allegiance == "dead"
>       then callSp conn "create_corpse"
>                   [ptype,
>                    (show x),
>                    (show y),
>                    show (PImaginary `elem` tags)]
>       else callSp conn "create_piece_internal"
>                   [ptype,
>                    allegiance,
>                    (show x),
>                    (show y),
>                    show (PImaginary `elem` tags)]
>     when (PUndead `elem` tags) $ do
>       tag <- selectValue conn "select max(tag) from pieces"
>       callSp conn "make_piece_undead" [ptype,allegiance, tag]
>   return () -}

this overrides the next random test by name e.g. so we can test the
board after a spell has succeeded and after it has failed

> rigActionSuccess :: Connection -> String -> Bool -> IO ()
> rigActionSuccess conn override setting = do
>   dbAction conn "rig_action_success" [override, show setting]
>   dbAction conn "reset_current_effects" []

================================================================================

= database update helpers

> {-startNewGame :: Connection -> IO ()
> startNewGame conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='human'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}

> {-startNewGameAI :: Connection -> IO ()
> startNewGameAI conn = do
>   dbAction conn "reset_new_game_widget_state"
>   runSql conn "update new_game_widget_state set state='computer'"
>   dbAction conn "client_new_game_using_new_game_widget_state"
>   dbAction conn "reset_current_effects"
>   checkNewGameRelvars conn-}


Adds the spell given to the first wizard's spell book

> addSpell :: Connection -> String -> IO ()
> addSpell conn spellName =
>   runSql conn "insert into spell_books (spell_name, wizard_name)\n\
>          \values (?, 'Buddha');" [spellName]

keep running next_phase until we get to the cast phase

> skipToPhase :: Connection -> [Char] -> IO ()
> skipToPhase conn phase = do
>   unless (phase `elem` ["choose","cast","move"])
>          (error $ "unrecognised phase: " ++ phase)
>   undefined
>   {-whenA1 (readTurnPhase conn)
>          (/= phase) $ do
>          sendKeyPress conn "space"
>          skipToPhase conn phase-}

> sendKeyPress :: Connection -> String -> IO ()
> sendKeyPress conn k = do
>   dbAction conn "key_pressed" [k]
>   dbAction conn "reset_current_effects" []

> rollbackOnError :: Connection -> IO c -> IO c
> rollbackOnError conn =
>     bracketOnError (return())
>                    (const $ runSql conn "rollback" []) . const

> startNewGameReadyToMove :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToMove conn board = do
>   startNewGame conn
>   setupBoard conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase conn "move"

> startNewGameReadyToMoveNoSpread :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToMoveNoSpread conn board = do
>   startNewGame conn
>   runSql conn "update disable_spreading_table\n\
>               \set disable_spreading = true;" []
>   setupBoard conn board
>   rigActionSuccess conn "disappear" False
>   skipToPhase conn "move"

> startNewGameReadyToAuto :: Connection -> BoardDiagram -> IO ()
> startNewGameReadyToAuto conn board = do
>   startNewGame conn
>   setupBoard conn board
>   sendKeyPress conn $ keyChooseSpell "disbelieve"
>   skipToPhase conn "cast"


================================================================================

= database read shortcuts

> readTurnPhase :: Connection -> IO String
> readTurnPhase conn = undefined
>   {-selectValue conn "select turn_phase from turn_phase_table"-}

> readCurrentWizard :: Connection -> IO String
> readCurrentWizard conn = undefined
>   {-selectValue conn
>          "select current_wizard from current_wizard_table"-}

> assertSelectedPiece :: Connection -> String -> String -> IO()
> assertSelectedPiece conn ptype allegiance = undefined {-do
>   t <- selectTuple conn "select ptype, allegiance from selected_piece"
>   assertEqual "selected piece"
>               (ptype,allegiance)
>               (lk "ptype" t, lk "allegiance" t)-}

> checkCurrentWizardPhase :: Connection -> String -> String -> IO()
> checkCurrentWizardPhase conn wiz phase = do
>   wiz' <- readCurrentWizard conn
>   phase' <- readTurnPhase conn
>   assertEqual "current wizard" wiz wiz'
>   assertEqual "current phase" phase' phase

> checkPieceDoneSelection :: Connection -> String -> String -> IO ()
> checkPieceDoneSelection conn ptype allegiance = undefined {-do
>     checkNoSelectedPiece conn
>     v <- selectTuples conn
>                       "select * from pieces_to_move\n\
>                       \where ptype=? and\n\
>                       \  allegiance=?" [ptype,allegiance]
>     assertBool "piece not in ptm" (null v)-}

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
> keyChooseSpell spellName = undefined {-safeLookup
>                              "get key for spell" spellName
>                              lookupChooseSpellKeys-}


===============================================================================

> startNewGame :: Connection -> IO ()
> startNewGame conn = do
>   newGame conn
>   resetCurrentEffects conn

> resetNewGameWidgetState conn = callSp conn "select action_reset_new_game_widget_state();" []

> setAllHuman conn = run conn "update new_game_widget_state set state='human';" []

> newGame conn = callSp conn "select action_client_new_game_using_new_game_widget_state" []

> resetCurrentEffects conn = callSp conn "select action_reset_current_effects" []

> callSp :: IConnection conn => conn -> String -> [String] -> IO ()
> callSp conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> runSql :: IConnection conn => conn -> String -> [String] -> IO ()
> runSql conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn

> dbAction :: IConnection conn => conn -> String -> [String] -> IO ()
> dbAction conn a args = callSp conn ("select action_" ++ a ++ "();") args

