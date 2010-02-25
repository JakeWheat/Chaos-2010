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

> {-# LANGUAGE FlexibleContexts,TemplateHaskell #-}
> module Games.Chaos2010.Tests.SetupGameState
>     (setupGame
>     ,setPhase
>     ,setCurrentWizard
>     ,addSpell
>     ,chooseSpell
>     ,B.wizardPiecesList
>     ,setWizards
>     ,useBoard
>     ,B.liftPl
>     ,B.wizardNames
>     ,diagramToRVs
>     ,B.BoardDiagram
>     ) where

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import qualified Games.Chaos2010.Tests.BoardUtils as B
> --import Games.Chaos2010.Tests.TestUtils
> --import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.HaskellDBUtils
> --import Games.Chaos2010.Tests.DBUpdates
> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.Tests.RelationalAlgebra as R

> -- tables
> import Games.Chaos2010.Database.Board_size
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_number_table
> import qualified Games.Chaos2010.Database.Current_wizard_table as Cw
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Database.Pieces
> import qualified Games.Chaos2010.Database.Imaginary_pieces as I
> import qualified Games.Chaos2010.Database.Crimes_against_nature as Cr
> import qualified Games.Chaos2010.Database.Spell_books as Sb
> import qualified Games.Chaos2010.Database.Cursor_position as C
> import qualified Games.Chaos2010.Database.Wizard_display_info as Wd
> import Games.Chaos2010.Database.Game_completed_table
> import Games.Chaos2010.Database.Cast_alignment_table
> import Games.Chaos2010.Database.Remaining_walk_table
> import qualified Games.Chaos2010.Database.Selected_piece as Sp
> import qualified Games.Chaos2010.Database.Pieces_moved as Pm
> import Games.Chaos2010.Database.Spell_parts_to_cast_table
> import qualified Games.Chaos2010.Database.Wizard_spell_choices_mr as Wc
> import qualified Games.Chaos2010.Database.Action_history_mr as Ah
> import Games.Chaos2010.Database.Cast_success_checked_table
> import Games.Chaos2010.Database.Test_action_overrides

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
>             ,currentWizard = (Cw.current_wizard .=. "Buddha"
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
>             ,cursorPosition = C.x .=. 0 .*. C.y .=. 0 .*. emptyRecord
>             ,wizardDisplayInfo = map (\(n,s,c) -> Wd.wizard_name .=. n
>                                                   .*. Wd.default_sprite .=. s
>                                                   .*. Wd.colour .=. c
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
>            (Sb.xid .=. i
>             .*. Sb.wizard_name .=. n
>             .*. Sb.spell_name .=. s
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
>     gs {currentWizard = (Cw.current_wizard .=. w
>                            .*. emptyRecord)}

> addSpell :: String -> String -> GameState -> GameState
> addSpell w s gs =
>     gs {spellBooks = entry : spellBooks gs}
>     where
>       entry = Sb.xid .=. 32409
>               .*. Sb.wizard_name .=. w
>               .*. Sb.spell_name .=. s
>               .*. emptyRecord

> chooseSpell :: String -> String -> Maybe Bool -> GameState -> GameState
> chooseSpell w s i gs = gs {
>   wizardSpellChoices = entry : wizardSpellChoices gs}
>   where
>     entry = Wc.wizard_name .=. w
>             .*. Wc.spell_name .=. s
>             .*. Wc.imaginary .=. i
>             .*. emptyRecord


set the wizards to the given list of names, by killing the others
basically does the foreign key cascading

> setWizards :: [String] -> GameState -> GameState
> setWizards wz gs =
>         gs {wezards = expireWizards $ wezards gs
>            ,currentWizard = Cw.current_wizard .=. (head wz)
>                             .*. emptyRecord
>            ,spellBooks = R.restrict (\r -> r # Sb.wizard_name `elem` wz) $ spellBooks gs
>            ,peeces = R.restrict (\r -> r # allegiance `elem` als) $ peeces gs
>            }
>   where
>     als = "dead" : wz
>     expireWizards = R.update (\r -> expired .=. True .@. r)
>                              (\r -> r # wizard_name `notElem` wz)

> useBoard :: B.BoardDiagram -> GameState -> GameState
> useBoard bd =
>   let (wzs,ps,ims,crs) = diagramToRVs bd
>   in (\gs ->
>       gs {peeces = ps
>          ,imaginaryPieces = ims
>          ,crimesAgainstNature = crs})
>       . setWizards wzs

> diagramToRVs :: B.BoardDiagram -> ([String]
>                                   ,[Pieces_v]
>                                   ,[Imaginary_pieces_v]
>                                   ,[Crimes_against_nature_v])
> diagramToRVs bd =
>   let (pdp,wzs) = B.parseBoardDiagram bd
>       pdpts = zip pdp [5..]
>   in (wzs
>      ,map makePiece pdpts
>      ,map makeI $ filter ((# B.imaginary) . fst) pdpts
>      ,map makeC $ filter ((# B.undead) . fst) pdpts)
>   where
>       makePiece :: (B.PieceDescriptionPos,Int) -> Pieces_v
>       makePiece (r,t) = ptype .=. (r # B.ptype)
>                         .*. allegiance .=. (r # B.allegiance)
>                         .*. tag .=. t
>                         .*. x .=. (r # B.x)
>                         .*. y .=. (r # B.y)
>                         .*. emptyRecord
>       makeI (r,t) = I.ptype .=. (r # B.ptype)
>                     .*. I.allegiance .=. (r # B.allegiance)
>                     .*. I.tag .=. t
>                     .*. emptyRecord
>       makeC (r,t) = Cr.ptype .=. (r # B.ptype)
>                     .*. Cr.allegiance .=. (r # B.allegiance)
>                     .*. Cr.tag .=. t
>                     .*. emptyRecord


> {-newGameReadyToCast :: IConnection conn =>
>                       Database
>                    -> conn
>                    -> String
>                    -> Maybe Bool
>                    -> GameState
>                    -> IO ()
> newGameReadyToCast db conn spellName im gs = do
>   setupGame db conn ((setPhase "choose"
>                      . setCurrentWizard "Zarathushthra"
>                      . addSpell "Buddha" spellName
>                      . addSpellChoice "Buddha" spellName im) gs)
>   nextPhase conn-}

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
>     setRelvarT db Cw.current_wizard_table $ currentWizard gs
>     setRelvarT db turn_phase_table $ turnPhase gs
>     setRelvar db pieces $ peeces gs
>     setRelvar db Sb.spell_books $ spellBooks gs
>     setRelvar db I.imaginary_pieces $ imaginaryPieces gs
>     setRelvar db Cr.crimes_against_nature $ crimesAgainstNature gs
>     setRelvarT db C.cursor_position $ cursorPosition gs
>     setRelvar db Wd.wizard_display_info $ wizardDisplayInfo gs
>     setRelvar db game_completed_table $ gameCompleted gs
>     setRelvar db Wc.wizard_spell_choices_mr $ wizardSpellChoices gs
>     {-if (turnPhase gs) # turn_phase == "cast"
>       then setRelvarT db cast_alignment_table $ cast_alignment .=. 0 .*. emptyRecord
>       else clearTable db cast_alignment_table-}
>     clearTable db cast_alignment_table
>     clearTable db remaining_walk_table
>     clearTable db Sp.selected_piece
>     clearTable db Pm.pieces_moved
>     clearTable db spell_parts_to_cast_table
>     clearTable db Ah.action_history_mr
>     clearTable db cast_success_checked_table
>     clearTable db test_action_overrides




> removeWizardN :: Int -> GameState -> GameState
> removeWizardN n gs =
>     let wizardName = (head (restrictTable (wezards gs)
>                                    (\r -> r # original_place == n)))
>                        # wizard_name
>         a = gs
>           {wezards = updateTable (wezards gs)
>                                  (\r -> r # original_place == n)
>                                  (\r -> expired .=. True .@. r)
>           ,peeces = restrictTable (peeces gs)
>                                   (\r -> r # allegiance /= wizardName)
>           ,spellBooks = restrictTable (spellBooks gs)
>                                       (\r -> r # Sb.wizard_name /= wizardName)
>           ,wizardDisplayInfo = restrictTable (wizardDisplayInfo gs)
>                                              (\r -> r # Wd.wizard_name /= wizardName)
>           }
>     in a
>           {currentWizard = let liveWizards = restrictTable (wezards a)
>                                                            (\r -> r # expired == False)
>                                f = (head liveWizards) # wizard_name
>                            in Cw.current_wizard .=. f
>                               .*. emptyRecord
>           }
>   where
>     updateTable t w u =
>         flip map t $ \r -> if w r
>                            then u r
>                            else r
>     restrictTable t w = flip filter t $ \r -> w r

> makePiece :: String -> String -> Int -> Int -> Int -> Pieces_v
> makePiece p a t xp yp = ptype .=. p
>                         .*. allegiance .=. a
>                         .*. tag .=. t
>                         .*. x .=. xp
>                         .*. y .=. yp
>                         .*. emptyRecord
> makeImag :: String -> String -> Int -> Imaginary_pieces_v
> makeImag p a t = I.ptype .=. p
>                  .*. I.allegiance .=. a
>                  .*. I.tag .=. t
>                  .*. emptyRecord
> makeCrime :: String -> String -> Int -> Crimes_against_nature_v
> makeCrime p a t = Cr.ptype .=. p
>                   .*. Cr.allegiance .=. a
>                   .*. Cr.tag .=. t
>                   .*. emptyRecord


