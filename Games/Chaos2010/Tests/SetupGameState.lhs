
Utility to help with tests, maybe use for save games as well?

Describe the complete game state by listing the contents of all
relevant tables. Use defaults to make this as concise as possible.

> {-# LANGUAGE FlexibleContexts #-}
> module Games.Chaos2010.Tests.SetupGameState
>     (defaultGameState
>     ,GameState(..)
>     ,setupGame
>     ,Wizards_v
>     ,Pieces_v
>     ,Imaginary_pieces_v
>     ,Crimes_against_nature_v
>     ) where

> --import Test.HUnit
> --import Test.Framework
> --import Control.Monad

> import Database.HaskellDB
> import Database.HaskellDB.Query
> import Database.HaskellDB.PrimQuery
> import Database.HDBC (IConnection)

> --import Games.Chaos2010.Tests.BoardUtils
> --import Games.Chaos2010.Tests.TestUtils
> --import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.DBUpdates
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
?       spell_parts_to_cast_table  | Chaos.Server.TurnSequence
?       cast_success_checked_table | Chaos.Server.TurnSequence
?       pieces_moved               | Chaos.Server.TurnSequence
?       selected_piece             | Chaos.Server.TurnSequence
?       remaining_walk_table       | Chaos.Server.TurnSequence

probably don't care about: (* useful for savegames)

 test_action_overrides      | Chaos.Server.Actions.TestSupport
* game_completed_table       | Chaos.Server.TurnSequence
 disable_spreading_table    | Chaos.Server.Actions.Autononmous
* action_history_mr          | Chaos.Server.Actions.History
* wizard_display_info        | Chaos.Client.WizardDisplayInfo
* cursor_position            | Chaos.Client.BoardWidget
* piece_starting_ticks       | Chaos.Client.BoardWidget
 spell_book_show_all_table  | Chaos.Client.SpellBookWidget
 new_game_widget_state      | Chaos.Client.NewGameWidget

the idea is to create a default value which is a set of haskelldb
records, one for each table. Then we can disable all the constraint
triggers, wipe then insert into each table, then reenable all the
triggers.

> type Board_size_v =
>    Record (HCons (LVPair Width Int)
>            (HCons (LVPair Height Int) HNil))

> type World_alignment_table_v =
>    Record (HCons (LVPair World_alignment Int) HNil)

> type Turn_number_table_v =
>    Record (HCons (LVPair Turn_number Int) HNil)

> type Current_wizard_table_v =
>    Record (HCons (LVPair Cw.Current_wizard String) HNil)

> type Turn_phase_table_v =
>    Record (HCons (LVPair Turn_phase String) HNil)

> type Wizards_v =
>     Record (HCons (LVPair Wizard_name String)
>             (HCons (LVPair Shadow_form Bool)
>              (HCons (LVPair Magic_sword Bool)
>               (HCons (LVPair Magic_knife Bool)
>                (HCons (LVPair Magic_shield Bool)
>                 (HCons (LVPair Magic_wings Bool)
>                  (HCons (LVPair Magic_armour Bool)
>                   (HCons (LVPair Magic_bow Bool)
>                    (HCons (LVPair Computer_controlled Bool)
>                     (HCons (LVPair Original_place Int)
>                      (HCons (LVPair Expired Bool) HNil)))))))))))

> type Pieces_v =
>    Record (HCons (LVPair Ptype String)
>            (HCons (LVPair Allegiance String)
>             (HCons (LVPair Tag Int)
>              (HCons (LVPair X Int)
>               (HCons (LVPair Y Int) HNil)))))

> type Spell_books_v =
>     Record (HCons (LVPair Sb.Id Int)
>             (HCons (LVPair Sb.Wizard_name String)
>              (HCons (LVPair Sb.Spell_name String) HNil)))

> type Imaginary_pieces_v =
>     Record (HCons (LVPair I.Ptype String)
>             (HCons (LVPair I.Allegiance String)
>              (HCons (LVPair I.Tag Int) HNil)))

> type Crimes_against_nature_v =
>     Record (HCons (LVPair Cr.Ptype String)
>             (HCons (LVPair Cr.Allegiance String)
>              (HCons (LVPair Cr.Tag Int) HNil)))

> type Cursor_position_v =
>     Record (HCons (LVPair C.X Int)
>             (HCons (LVPair C.Y Int) HNil))

> type Wizard_display_info_v =
>     Record (HCons (LVPair Wd.Wizard_name String)
>             (HCons (LVPair Wd.Default_sprite String)
>              (HCons (LVPair Wd.Colour String) HNil)))



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
>     }

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
>             ,wezards = let defaults n p = wizard_name .=. n
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
>                        in map (uncurry defaults) $ zip
>                               ["Buddha"
>                               ,"Kong Fuzi"
>                               ,"Laozi"
>                               ,"Moshe"
>                               ,"Muhammad"
>                               ,"Shiva"
>                               ,"Yeshua"
>                               ,"Zarathushthra"] [0..]
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
>             }

> setupGame :: IConnection conn => Database -> conn -> GameState -> IO ()
> setupGame db conn gs = withConstraintsDisabled conn $ transaction db $ do
>   setRelvarT db board_size $ boardSize gs
>   setRelvarT db world_alignment_table $ worldAlignment gs
>   setRelvarT db turn_number_table $ turnNumber gs
>   setRelvar db wizards $ wezards gs
>   setRelvarT db Cw.current_wizard_table $ currentWizard gs
>   setRelvarT db turn_phase_table $ turnPhase gs
>   setRelvar db pieces $ peeces gs
>   setRelvar db Sb.spell_books $ spellBooks gs
>   setRelvar db I.imaginary_pieces $ imaginaryPieces gs
>   setRelvar db Cr.crimes_against_nature $ crimesAgainstNature gs
>   setRelvarT db C.cursor_position $ cursorPosition gs
>   setRelvar db Wd.wizard_display_info $ wizardDisplayInfo gs

> setRelvarT :: (RecordLabels er ls,
>               HLabelSet ls,
>               HRearrange ls r r',
>               RecordValues r' vs',
>               HMapOut
>               ToPrimExprsOp vs' Database.HaskellDB.PrimQuery.PrimExpr,
>               InsertRec r' er,
>               HMap ConstantRecordOp r1 r) =>
>              Database -> Table (Record er) -> Record r1 -> IO ()
> setRelvarT db t v = do
>   clearTable db t
>   insert db t $ constantRecord v

> setRelvar :: (RecordLabels er ls,
>               HLabelSet ls,
>               HRearrange ls r r',
>               RecordValues r' vs',
>               HMapOut ToPrimExprsOp vs' PrimExpr,
>               InsertRec r' er,
>               HMap ConstantRecordOp r1 r) =>
>              Database -> Table (Record er) -> [Record r1] -> IO ()
> setRelvar db t v = do
>   clearTable db t
>   forM_ v $ insert db t . constantRecord


> clearTable :: Database -> Table r -> IO ()
> clearTable db t =
>    delete db t (const $ constant True)

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

