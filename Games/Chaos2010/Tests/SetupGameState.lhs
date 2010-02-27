Copyright 2010 Jake Wheat

Utility to help with setting up new games.

The idea is to start with a default game state which is a set of
[recs], one for each data table, e.g. a list for the wizards table,
pieces table, etc.. There is a routine to initialize the database from
this collection, which does this by disabling all the constraints,
wiping then loading all the tables.

This is then modified by a bunch of operators which can change this
default game state, e.g so we can start with a default starting game,
then say, add goblin spell to the first wizard's spell book, make it
his choice, and start in the cast phase.

Some of these operators just modify the default game state value, but
some need to be more clever - if we want to remove a wizard this is a
bit tedious since we don't have cascading foreign keys in the default
game state type, if we want to set the turn phase to cast or somewhere
in the middle of a piece's move, there is a bunch of additional state
to setup in various tables - want to do this automatically.

> {-# LANGUAGE FlexibleContexts,TemplateHaskell,ScopedTypeVariables #-}
> module Games.Chaos2010.Tests.SetupGameState
>     (setupGame
>     ,setPhase
>     ,setCurrentWizard
>     ,addSpell
>     ,wChooseSpell
>     ,wizardPiecesList
>     ,setWizards
>     ,useBoard
>     ,liftPl
>     ,makePD
>     ,makeFPD
>     ,wizardNames
>     ,PieceDescription
>     ,parseBoardDiagram
>     ,PieceDescriptionPos
>     ,BoardDiagram
>     ,assertPiecesEquals
>     ,assertPiecesOnTopEquals
>     ,assertValidSquaresEquals
>     ,addAndChoose
>     ,readyToCast
>     ) where

> import Database.HaskellDB
> import Database.HDBC (IConnection)
> import Test.HUnit
> import Control.Applicative

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.HaskellDBUtils
> import qualified Games.Chaos2010.Tests.RelationalAlgebra as R

> -- tables
> import Games.Chaos2010.Database.Board_size
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Database.Pieces
> import Games.Chaos2010.Database.Pieces_mr
> import Games.Chaos2010.Database.Pieces_on_top_view
> import Games.Chaos2010.Database.Fields
> import Games.Chaos2010.Database.Imaginary_pieces
> import Games.Chaos2010.Database.Crimes_against_nature
> import Games.Chaos2010.Database.Spell_books
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.Database.Wizard_display_info
> import Games.Chaos2010.Database.Game_completed_table
> import Games.Chaos2010.Database.Cast_alignment_table
> import Games.Chaos2010.Database.Remaining_walk_table
> import Games.Chaos2010.Database.Selected_piece
> import Games.Chaos2010.Database.Pieces_moved
> import Games.Chaos2010.Database.Spell_parts_to_cast_table
> import Games.Chaos2010.Database.Wizard_spell_choices_mr
> import Games.Chaos2010.Database.Action_history_mr
> import Games.Chaos2010.Database.Cast_success_checked_table
> import Games.Chaos2010.Database.Test_action_overrides
> import Games.Chaos2010.Database.Current_wizard_spell_squares
> import Games.Chaos2010.Database.Target_spells
> import Games.Chaos2010.Database.Current_wizard_spell

> import Games.Chaos2010.Tests.TableValueTypes

= game state

tables to possibly set:

single  board_size                 | Chaos.Server.GlobalData
single  world_alignment_table      | Chaos.Server.GlobalData

single  turn_number_table          | Chaos.Server.TurnSequence
single  current_wizard_table       | Chaos.Server.TurnSequence
single  turn_phase_table           | Chaos.Server.TurnSequence

pieces  pieces                     | Chaos.Server.Pieces
pieces  imaginary_pieces           | Chaos.Server.Pieces
pieces  crimes_against_nature      | Chaos.Server.Pieces
pieces  wizards                    | Chaos.Server.Wizards
?       spell_books                | Chaos.Server.Wizards
?       wizard_spell_choices_mr    | Chaos.Server.TurnSequence

?       pieces_moved               | Chaos.Server.TurnSequence
?       selected_piece             | Chaos.Server.TurnSequence

?       wizard_display_info        | Chaos.Client.WizardDisplayInfo
single  cursor_position            | Chaos.Client.BoardWidget

these are the tricky ones which get set with all sorts of weird values during the turn progression. We don't want the test code to have to worry about all this, so we just provide combinators to set the turn phase or sub phase, and all this is handled automatically

?       spell_parts_to_cast_table  | Chaos.Server.TurnSequence
?       cast_success_checked_table | Chaos.Server.TurnSequence
?       remaining_walk_table       | Chaos.Server.TurnSequence
cast_alignment_table

 test_action_overrides      | Chaos.Server.Actions.TestSupport
* game_completed_table       | Chaos.Server.TurnSequence
 disable_spreading_table    | Chaos.Server.Actions.Autononmous
* action_history_mr          | Chaos.Server.Actions.History
* piece_starting_ticks       | Chaos.Client.BoardWidget
 spell_book_show_all_table  | Chaos.Client.SpellBookWidget
 new_game_widget_state      | Chaos.Client.NewGameWidget

== the GameState type and defaultGameState value

> data GameState = GameState
>     {boardSize :: Board_size_v
>     ,worldAlignment :: World_alignment_table_v
>     ,turnNumber :: Turn_number_table_v
>     ,currentWizard :: Current_wizard_table_v
>     ,turnPhase :: Turn_phase_table_v
>     ,wezards :: [Wizards_v]
>     ,peeces :: [Pieces_v]
>     ,spellBooks :: [Spell_books_v]
>     ,imaginaryPieces :: [Imaginary_pieces_v]
>     ,crimesAgainstNature :: [Crimes_against_nature_v]
>     ,cursorPosition :: Cursor_position_v
>     ,wizardDisplayInfo :: [Wizard_display_info_v]
>     ,gameCompleted :: [Game_completed_table_v]
>     ,wizardSpellChoices :: [Wizard_spell_choices_mr_v]
>     }
>                  deriving Show

> defaultGameState :: GameState
> defaultGameState =
>   GameState {boardSize = (width .=. 15
>                           .*. height .=. 10
>                           .*. emptyRecord)
>             ,worldAlignment = (world_alignment .=. 0
>                                .*. emptyRecord)
>             ,turnNumber = (turn_number .=. 0
>                            .*. emptyRecord)
>             ,currentWizard = (current_wizard .=. "Buddha"
>                            .*. emptyRecord)
>             ,turnPhase = (turn_phase .=. "choose"
>                            .*. emptyRecord)
>             ,wezards = defaultWizardList
>             ,peeces = map (\(p,a,t,xp,yp) ->
>                                ptype .=. p
>                            .*. allegiance .=. a
>                            .*. tag .=. t
>                            .*. x .=. xp
>                            .*. y .=. yp
>                            .*. emptyRecord)
>                        [("wizard","Buddha",0,0,0)
>                        ,("wizard","Kong Fuzi",0,7,0)
>                        ,("wizard","Laozi",0,14,0)
>                        ,("wizard","Moshe",0,0,4)
>                        ,("wizard","Muhammad",0,14,4)
>                        ,("wizard","Shiva",0,0,9)
>                        ,("wizard","Yeshua",0,7,9)
>                        ,("wizard","Zarathushthra",0,14,9)]
>             ,spellBooks = defaultSpellBookValue
>             ,imaginaryPieces = []
>             ,crimesAgainstNature = []
>             ,cursorPosition = x .=. 0 .*. y .=. 0 .*. emptyRecord
>             ,wizardDisplayInfo = map (\(n,s,c) -> wizard_name .=. n
>                                                   .*. default_sprite .=. s
>                                                   .*. colour .=. c
>                                                   .*. emptyRecord)
>                                      [("Buddha", "wizard0", "blue")
>                                      ,("Kong Fuzi", "wizard1", "purple")
>                                      ,("Laozi", "wizard2", "cyan")
>                                      ,("Moshe", "wizard3", "yellow")
>                                      ,("Muhammad", "wizard4", "green")
>                                      ,("Shiva", "wizard5", "red")
>                                      ,("Yeshua", "wizard6", "white")
>                                      ,("Zarathushthra", "wizard7", "orange")]
>             ,gameCompleted = []
>             ,wizardSpellChoices = []
>             }

> defaultWizardList :: [Wizards_v]
> defaultWizardList = let defaults n p = wizard_name .=. n
>                                           .*. shadow_form .=. False
>                                           .*. magic_sword .=. False
>                                           .*. magic_knife .=. False
>                                           .*. magic_shield .=. False
>                                           .*. magic_wings .=. False
>                                           .*. magic_armour .=. False
>                                           .*. magic_bow .=. False
>                                           .*. computer_controlled .=. False
>                                           .*. original_place .=. p
>                                           .*. expired .=. False
>                                           .*. emptyRecord
>                     in map (uncurry defaults) $ flip zip [0..]
>                               ["Buddha"
>                               ,"Kong Fuzi"
>                               ,"Laozi"
>                               ,"Moshe"
>                               ,"Muhammad"
>                               ,"Shiva"
>                               ,"Yeshua"
>                               ,"Zarathushthra"]

> defaultSpellBookValue :: [Spell_books_v]
> defaultSpellBookValue =
>   map (\(i,n,s) ->
>            (xid .=. i
>             .*. wizard_name .=. n
>             .*. spell_name .=. s
>             .*. emptyRecord))
>     [(115362, "Buddha", "disbelieve")
>     ,(115363, "Kong Fuzi", "disbelieve")
>     ,(115364, "Laozi", "disbelieve")
>     ,(115365, "Moshe", "disbelieve")
>     ,(115366, "Muhammad", "disbelieve")
>     ,(115367, "Shiva", "disbelieve")
>     ,(115368, "Yeshua", "disbelieve")
>     ,(115369, "Zarathushthra", "disbelieve")
>     ,(115370, "Buddha", "giant")
>     ,(115371, "Buddha", "magic_sword")
>     ,(115372, "Buddha", "law")
>     ,(115373, "Buddha", "magic_shield")
>     ,(115374, "Buddha", "dark_power")
>     ,(115375, "Buddha", "unicorn")
>     ,(115376, "Buddha", "law")
>     ,(115377, "Buddha", "gorilla")
>     ,(115378, "Buddha", "wall")
>     ,(115379, "Buddha", "justice")
>     ,(115380, "Buddha", "pegasus")
>     ,(115381, "Buddha", "chaos")
>     ,(115382, "Buddha", "shadow_form")
>     ,(115383, "Buddha", "golden_dragon")
>     ,(115384, "Buddha", "giant")
>     ,(115385, "Buddha", "skeleton")
>     ,(115386, "Buddha", "magic_armour")
>     ,(115387, "Buddha", "decree")
>     ,(115388, "Buddha", "gorilla")
>     ,(115389, "Kong Fuzi", "shadow_form")
>     ,(115390, "Kong Fuzi", "dark_citadel")
>     ,(115391, "Kong Fuzi", "subversion")
>     ,(115392, "Kong Fuzi", "shadow_form")
>     ,(115393, "Kong Fuzi", "vengeance")
>     ,(115394, "Kong Fuzi", "gryphon")
>     ,(115395, "Kong Fuzi", "magic_fire")
>     ,(115396, "Kong Fuzi", "harpy")
>     ,(115397, "Kong Fuzi", "gryphon")
>     ,(115398, "Kong Fuzi", "shadow_wood")
>     ,(115399, "Kong Fuzi", "spectre")
>     ,(115400, "Kong Fuzi", "faun")
>     ,(115401, "Kong Fuzi", "wraith")
>     ,(115402, "Kong Fuzi", "lion")
>     ,(115403, "Kong Fuzi", "horse")
>     ,(115404, "Kong Fuzi", "magic_knife")
>     ,(115405, "Kong Fuzi", "magic_bolt")
>     ,(115406, "Kong Fuzi", "law")
>     ,(115407, "Kong Fuzi", "shadow_wood")
>     ,(115408, "Laozi", "red_dragon")
>     ,(115409, "Laozi", "goblin")
>     ,(115410, "Laozi", "golden_dragon")
>     ,(115411, "Laozi", "magic_bow")
>     ,(115412, "Laozi", "green_dragon")
>     ,(115413, "Laozi", "dark_power")
>     ,(115414, "Laozi", "pegasus")
>     ,(115415, "Laozi", "lightning")
>     ,(115416, "Laozi", "subversion")
>     ,(115417, "Laozi", "law")
>     ,(115418, "Laozi", "magic_armour")
>     ,(115419, "Laozi", "ghost")
>     ,(115420, "Laozi", "dark_power")
>     ,(115421, "Laozi", "magic_armour")
>     ,(115422, "Laozi", "dark_citadel")
>     ,(115423, "Laozi", "skeleton")
>     ,(115424, "Laozi", "large_law")
>     ,(115425, "Laozi", "harpy")
>     ,(115426, "Laozi", "king_cobra")
>     ,(115427, "Moshe", "red_dragon")
>     ,(115428, "Moshe", "magic_sword")
>     ,(115429, "Moshe", "law")
>     ,(115430, "Moshe", "magic_sword")
>     ,(115431, "Moshe", "pegasus")
>     ,(115432, "Moshe", "golden_dragon")
>     ,(115433, "Moshe", "chaos")
>     ,(115434, "Moshe", "elf")
>     ,(115435, "Moshe", "shadow_form")
>     ,(115436, "Moshe", "magic_sword")
>     ,(115437, "Moshe", "harpy")
>     ,(115438, "Moshe", "magic_sword")
>     ,(115439, "Moshe", "large_chaos")
>     ,(115440, "Moshe", "magic_bolt")
>     ,(115441, "Moshe", "spectre")
>     ,(115442, "Moshe", "wall")
>     ,(115443, "Moshe", "shadow_wood")
>     ,(115444, "Moshe", "subversion")
>     ,(115445, "Moshe", "manticore")
>     ,(115446, "Muhammad", "gorilla")
>     ,(115447, "Muhammad", "pegasus")
>     ,(115448, "Muhammad", "vampire")
>     ,(115449, "Muhammad", "red_dragon")
>     ,(115450, "Muhammad", "unicorn")
>     ,(115451, "Muhammad", "wall")
>     ,(115452, "Muhammad", "green_dragon")
>     ,(115453, "Muhammad", "vampire")
>     ,(115454, "Muhammad", "red_dragon")
>     ,(115455, "Muhammad", "orc")
>     ,(115456, "Muhammad", "law")
>     ,(115457, "Muhammad", "goblin")
>     ,(115458, "Muhammad", "magic_bolt")
>     ,(115459, "Muhammad", "spectre")
>     ,(115460, "Muhammad", "large_law")
>     ,(115461, "Muhammad", "ghost")
>     ,(115462, "Muhammad", "magic_knife")
>     ,(115463, "Muhammad", "magic_wood")
>     ,(115464, "Muhammad", "gooey_blob")
>     ,(115465, "Shiva", "manticore")
>     ,(115466, "Shiva", "lightning")
>     ,(115467, "Shiva", "subversion")
>     ,(115468, "Shiva", "dark_citadel")
>     ,(115469, "Shiva", "chaos")
>     ,(115470, "Shiva", "green_dragon")
>     ,(115471, "Shiva", "magic_bow")
>     ,(115472, "Shiva", "wall")
>     ,(115473, "Shiva", "law")
>     ,(115474, "Shiva", "horse")
>     ,(115475, "Shiva", "skeleton")
>     ,(115476, "Shiva", "vampire")
>     ,(115477, "Shiva", "magic_sword")
>     ,(115478, "Shiva", "magic_knife")
>     ,(115479, "Shiva", "skeleton")
>     ,(115480, "Shiva", "king_cobra")
>     ,(115481, "Shiva", "lion")
>     ,(115482, "Shiva", "red_dragon")
>     ,(115483, "Shiva", "ogre")
>     ,(115484, "Yeshua", "justice")
>     ,(115485, "Yeshua", "magic_castle")
>     ,(115486, "Yeshua", "large_law")
>     ,(115487, "Yeshua", "magic_sword")
>     ,(115488, "Yeshua", "raise_dead")
>     ,(115489, "Yeshua", "wall")
>     ,(115490, "Yeshua", "magic_armour")
>     ,(115491, "Yeshua", "golden_dragon")
>     ,(115492, "Yeshua", "elf")
>     ,(115493, "Yeshua", "chaos")
>     ,(115494, "Yeshua", "subversion")
>     ,(115495, "Yeshua", "harpy")
>     ,(115496, "Yeshua", "red_dragon")
>     ,(115497, "Yeshua", "harpy")
>     ,(115498, "Yeshua", "goblin")
>     ,(115499, "Yeshua", "red_dragon")
>     ,(115500, "Yeshua", "large_law")
>     ,(115501, "Yeshua", "magic_armour")
>     ,(115502, "Yeshua", "gryphon")
>     ,(115503, "Zarathushthra", "justice")
>     ,(115504, "Zarathushthra", "orc")
>     ,(115505, "Zarathushthra", "magic_wood")
>     ,(115506, "Zarathushthra", "golden_dragon")
>     ,(115507, "Zarathushthra", "magic_wings")
>     ,(115508, "Zarathushthra", "gryphon")
>     ,(115509, "Zarathushthra", "shadow_wood")
>     ,(115510, "Zarathushthra", "lion")
>     ,(115511, "Zarathushthra", "orc")
>     ,(115512, "Zarathushthra", "green_dragon")
>     ,(115513, "Zarathushthra", "gooey_blob")
>     ,(115514, "Zarathushthra", "large_chaos")
>     ,(115515, "Zarathushthra", "manticore")
>     ,(115516, "Zarathushthra", "shadow_wood")
>     ,(115517, "Zarathushthra", "spectre")
>     ,(115518, "Zarathushthra", "gryphon")
>     ,(115519, "Zarathushthra", "magic_knife")
>     ,(115520, "Zarathushthra", "shadow_form")
>     ,(115521, "Zarathushthra", "shadow_form")]

===============================================================================

the combinators for altering the default game state value


> setPhase :: String -> GameState -> GameState
> setPhase p gs =
>     gs {turnPhase = (turn_phase .=. p .*. emptyRecord)}


> setCurrentWizard :: String -> GameState -> GameState
> setCurrentWizard w gs =
>     gs {currentWizard = (current_wizard .=. w
>                            .*. emptyRecord)}

> addSpell :: String -> String -> GameState -> GameState
> addSpell w s gs =
>     gs {spellBooks = entry : spellBooks gs}
>     where
>       entry = xid .=. 32409
>               .*. wizard_name .=. w
>               .*. spell_name .=. s
>               .*. emptyRecord

> wChooseSpell :: String -> String -> Maybe Bool -> GameState -> GameState
> wChooseSpell w s i gs = gs {
>   wizardSpellChoices = entry : wizardSpellChoices gs}
>   where
>     entry = wizard_name .=. w
>             .*. spell_name .=. s
>             .*. imaginary .=. i
>             .*. emptyRecord

> readyToCast :: String -> [GameState -> GameState]
> readyToCast s = [addSpell "Buddha" s
>                 ,wChooseSpell "Buddha" s Nothing
>                 ,setPhase "cast"]

> addAndChoose :: String -> [GameState -> GameState]
> addAndChoose s = [addSpell "Buddha" s
>                  ,wChooseSpell "Buddha" s Nothing]


set the wizards to the given list of names, by killing the others
basically does the foreign key cascading

> setWizards :: [String] -> GameState -> GameState
> setWizards wz gs =
>         gs {wezards = expireWizards $ wezards gs
>            ,currentWizard = if currentWizardDead
>                             then current_wizard .=. (head wz)
>                                      .*. emptyRecord
>                             else currentWizard gs
>            ,spellBooks = R.restrict (\r -> r # wizard_name `elem` wz) $ spellBooks gs
>            ,peeces = R.restrict (\r -> r # allegiance `elem` als) $ peeces gs
>            }
>   where
>     currentWizardDead = ((currentWizard gs) # current_wizard) `notElem` wz
>     als = "dead" : wz
>     expireWizards = R.update (\r -> expired .=. True .@. r)
>                              (\r -> r # wizard_name `notElem` wz)

> useBoard :: BoardDiagram -> GameState -> GameState
> useBoard bd =
>   let (wzs,ps,ims,crs) = diagramToRVs bd
>   in (\gs ->
>       gs {peeces = ps
>          ,imaginaryPieces = ims
>          ,crimesAgainstNature = crs})
>       . setWizards wzs

> diagramToRVs :: BoardDiagram -> ([String]
>                                   ,[Pieces_v]
>                                   ,[Imaginary_pieces_v]
>                                   ,[Crimes_against_nature_v])
> diagramToRVs bd =
>   let (wzs,pdp) = parseBoardDiagram bd
>       pdpts = zip pdp [5..]
>   in (wzs
>      ,map makePieceT pdpts
>      ,map makePat $ filter ((# imaginary) . fst) pdpts
>      ,map makePat $ filter ((# undead) . fst) pdpts)
>   where
>       makePieceT :: (PieceDescriptionPos,Int) -> Pieces_v
>       makePieceT (r,t) = ptype .=. (r # ptype)
>                         .*. allegiance .=. (r # allegiance)
>                         .*. tag .=. t
>                         .*. x .=. (r # x)
>                         .*. y .=. (r # y)
>                         .*. emptyRecord
>       makePat (r,t) = ptype .=. (r # ptype)
>                     .*. allegiance .=. (r # allegiance)
>                     .*. tag .=. t
>                     .*. emptyRecord

===============================================================================

the function which takes the final game state and sets the database up
using it

> setupGame :: IConnection conn => Database -> conn -> [(GameState -> GameState)] -> IO ()
> setupGame db conn ls = setupGame1 db conn $ applyEm ls defaultGameState

> applyEm :: [a->a] -> a -> a
> applyEm (f:fs) a = applyEm fs $ f a
> applyEm [] a = a

> setupGame1 :: IConnection conn => Database -> conn -> GameState -> IO ()
> setupGame1 db conn gs = withConstraintsDisabled conn $ transaction db $ do
>     setRelvarT db board_size $ boardSize gs
>     setRelvarT db world_alignment_table $ worldAlignment gs
>     setRelvarT db turn_number_table $ turnNumber gs
>     setRelvar db wizards $ wezards gs
>     setRelvarT db current_wizard_table $ currentWizard gs
>     setRelvarT db turn_phase_table $ turnPhase gs
>     setRelvar db pieces $ peeces gs
>     setRelvar db spell_books $ spellBooks gs
>     setRelvar db imaginary_pieces $ imaginaryPieces gs
>     setRelvar db crimes_against_nature $ crimesAgainstNature gs
>     setRelvarT db cursor_position $ cursorPosition gs
>     setRelvar db wizard_display_info $ wizardDisplayInfo gs
>     setRelvar db game_completed_table $ gameCompleted gs
>     setRelvar db wizard_spell_choices_mr $ wizardSpellChoices gs
>     if (turnPhase gs) # turn_phase == "cast"
>       then do
>         setRelvarT db cast_alignment_table $ cast_alignment .=. 0 .*. emptyRecord
>         sptc <- getMaybeSingleValue <$> query db (do
>                   ts <- table target_spells
>                   cws <- table current_wizard_spell
>                   restrict $ ts # spell_name .==. cws # spell_name
>                   project $ xcount .=. fn 0 (ts # numb) .*. emptyRecord)
>         setRelvarT db spell_parts_to_cast_table $ spell_parts_to_cast .=. maybe 0 id sptc .*. emptyRecord
>         setRelvarT db cast_success_checked_table $ cast_success_checked .=. False .*. emptyRecord
>       else do
>         clearTable db cast_alignment_table
>         clearTable db spell_parts_to_cast_table
>         clearTable db cast_success_checked_table

>     clearTable db remaining_walk_table
>     clearTable db selected_piece
>     clearTable db pieces_moved
>     clearTable db action_history_mr
>     clearTable db test_action_overrides

> assertPiecesOnTopEquals :: Database
>                    -> BoardDiagram
>                    -> IO ()
> assertPiecesOnTopEquals db dg = do
>   let (_,pdp) = parseBoardDiagram dg
>   assertRelvarValue db (do
>                          t <- table pieces_on_top_view
>                          projectNNPieces t) pdp

> type Nnp  = $(makeExprRecord [("Ptype", "String")
>                             ,("Allegiance", "String")
>                             ,("X", "Int")
>                             ,("Y", "Int")
>                             ,("Imaginary", "Bool")
>                             ,("Undead", "Bool")])


> projectNNPieces :: (HasField Undead r (Expr (Maybe Bool)),
>                     HasField Imaginary r (Expr (Maybe Bool)),
>                     HasField Allegiance r (Expr (Maybe String)),
>                     HasField X r (Expr (Maybe Int)),
>                     HasField Y r (Expr (Maybe Int)),
>                     HasField Ptype r (Expr (Maybe String))) =>
>                    r -> Query
>                         (Rel Nnp )
> projectNNPieces t = project $ ptype .=. fn "" (t # ptype)
>                                      .*. allegiance  .=. fn "" (t # allegiance)
>                                      .*. x .=. fn 0 (t # x)
>                                      .*. y .=. fn 0 (t # y)
>                                      .*. imaginary .=. fn False (t # imaginary)
>                                      .*. undead .=. fn False (t # undead)
>                                      .*. emptyRecord

> assertPiecesEquals :: Database
>                    -> BoardDiagram
>                    -> IO ()
> assertPiecesEquals db dg = do
>   let (_,pdp) = parseBoardDiagram dg
>   assertRelvarValue db (do
>                          t <- table pieces_mr
>                          projectNNPieces t) pdp

> assertValidSquaresEquals :: Database
>                          -> String
>                          -> IO ()
> assertValidSquaresEquals db vss = do
>     --get our piece tuple lists for the board in the database
>     --and the expected board
>     currentValidSquares <- queryValidSquares db
>     assertBool "valid squares wrong" $ recsEq currentValidSquares $ parseValidSquares vss

> queryValidSquares :: Database -> IO [Pos]
> queryValidSquares db =
>   query db $ do
>            tb <- table current_wizard_spell_squares
>            project $ x .=. fn (-1) (tb # x)
>                      .*. y .=. fn (-1) (tb # y)
>                      .*. emptyRecord
