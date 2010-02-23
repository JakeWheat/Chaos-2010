
Utility to help with tests, maybe use for save games as well?

Describe the complete game state by listing the contents of all
relevant tables. Use defaults to make this as concise as possible.

> {-# LANGUAGE FlexibleContexts #-}
> module Games.Chaos2010.Tests.SetupGameState where

> import Test.HUnit
> import Test.Framework
> import Control.Monad

> import Database.HaskellDB
> import Database.HaskellDB.Query
> import Database.HaskellDB.PrimQuery
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Database.Board_size

tables to possibly set:

single  board_size                 | Chaos.Server.GlobalData
single  world_alignment_table      | Chaos.Server.GlobalData
pieces  wizards                    | Chaos.Server.Wizards
?       spell_books                | Chaos.Server.Wizards
pieces  pieces                     | Chaos.Server.Pieces
pieces  imaginary_pieces           | Chaos.Server.Pieces
pieces  crimes_against_nature      | Chaos.Server.Pieces
single  turn_number_table          | Chaos.Server.TurnSequence
single  current_wizard_table       | Chaos.Server.TurnSequence
single  turn_phase_table           | Chaos.Server.TurnSequence
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

> data GameState = GameState {
>       boardSize :: Board_size_v
>       }

> defaultGameState :: GameState
> defaultGameState =
>   GameState {boardSize = (width .=. 15
>                           .*. height .=. 10
>                           .*. emptyRecord)
>             }

> setupGame :: IConnection conn => Database -> conn -> GameState -> IO ()
> setupGame db conn gs = withConstraintsDisabled conn $ do
>   setRelvar db board_size $ boardSize gs

> setRelvar :: (RecordLabels er ls,
>               HLabelSet ls,
>               HRearrange ls r r',
>               RecordValues r' vs',
>               HMapOut
>               ToPrimExprsOp vs' Database.HaskellDB.PrimQuery.PrimExpr,
>               InsertRec r' er,
>               HMap ConstantRecordOp r1 r) =>
>              Database -> Table (Record er) -> Record r1 -> IO ()
> setRelvar db t v = do
>   clearTable db t
>   insert db t $ constantRecord v

> clearTable :: Database -> Table r -> IO ()
> clearTable db t =
>    delete db t (const $ constant True)
