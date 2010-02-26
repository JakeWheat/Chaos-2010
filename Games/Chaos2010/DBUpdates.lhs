> module Games.Chaos2010.DBUpdates
>     (resetNewGameWidgetState
>     ,newGame
>     --,sendKeyPress
>     ,rollback
>     ,updateNewGameState
>     --testing stuff
>     ,setCursorPos
>     ,setAllHuman
>     ,killWizard
>     ,setWizardPosition
>     ,createCorpse
>     ,createPieceInternal
>     ,addSpell1
>     ,rigActionSuccess
>     ,disableSpreading
>     ,addMagicSword
>     ,killTopPieceAt
>     ,setupTestBoard
>     ,withConstraintsDisabled
>     ,nextPhaseChooseIf
>     ,nextPhase
>     ,skipToPhase
>     ,actionGo
>     ,actionMoveCursor
>     ,CursorDirection(..)
>     --,disableConstraints
>     --,enableConstraints
>     ) where
> import Database.HDBC hiding (rollback)
> import Database.HaskellDB
> import Data.List (intersperse)
> --import Database.HaskellDB.Query
> import Control.Exception


> import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Database.New_game_widget_state
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.Database.Pieces
> import Games.Chaos2010.Database.Fields
> import Games.Chaos2010.Database.Disable_spreading_table


> setupTestBoard :: IConnection conn => conn -> String -> IO ()
> setupTestBoard conn t =
>     callSp conn "action_setup_test_board" [t]

> updateNewGameState :: IConnection conn => Database -> conn -> Int -> String -> IO ()
> updateNewGameState db _ l s =
>   transaction db $
>     update db new_game_widget_state
>       (\r -> r # line .==. constant l)
>       (\_ -> state .=. constant s .*. emptyRecord)

>   --  runSql conn "update new_game_widget_state\n\
>   --              \set state =? where line =?" [s, show l]


> resetNewGameWidgetState :: IConnection conn => conn -> IO ()
> resetNewGameWidgetState conn = callSp conn "action_reset_new_game_widget_state" []

> setAllHuman :: IConnection conn => Database -> conn -> IO ()
> setAllHuman db _ =
>   transaction db $
>     update db new_game_widget_state
>       (\_ -> constant True)
>       (\_ -> state .=. constant "human" .*. emptyRecord)
>   --runSql conn "update new_game_widget_state set state='human';" []

> newGame :: IConnection conn => conn -> IO ()
> newGame conn = callSp conn "action_client_new_game_using_new_game_widget_state" []

> setCursorPos :: IConnection conn => Database -> conn -> Int -> Int -> IO ()
> setCursorPos db _ xp yp =
>   transaction db $
>     update db cursor_position
>       (\_ -> constant True)
>       (\_ -> x .=. constant xp .*. y .=. constant yp .*. emptyRecord)
>   --runSql conn "update cursor_position set x=?, y=?" [show xp,show yp]

> killWizard :: IConnection conn => conn -> String -> IO ()
> killWizard conn wiz =
>       callSp conn "kill_wizard" [wiz]

> setWizardPosition :: IConnection conn => Database -> conn -> String -> Int -> Int -> IO ()
> setWizardPosition db _ allegiance' xp yp =
>   transaction db $
>     update db pieces
>       (\r ->  (r # ptype .==. constant "wizard")
>              .&&. (r # allegiance .==. constant allegiance'))
>       (\_ -> x .=. constant xp
>              .*. y .=. constant yp
>              .*. emptyRecord)
>            --runSql conn "update pieces set x = ?, y = ?\n\
>            --             \where ptype='wizard' and allegiance=?"
>            --        [show xp, show yp, allegiance]

> createCorpse :: IConnection conn => conn -> String -> Int -> Int -> Bool -> IO ()
> createCorpse conn ptype' xp yp im =
>           callSp conn "create_corpse"
>                   [ptype',
>                    (show xp),
>                    (show yp),
>                    show im]

> createPieceInternal :: IConnection conn => conn -> String -> String -> Int -> Int -> Bool -> Bool -> IO ()
> createPieceInternal conn ptype' allegiance' xp yp im un = do
>     callSp conn "create_piece_internal"
>                   [ptype'
>                   ,allegiance'
>                   ,(show xp)
>                   ,(show yp)
>                   ,show im
>                   ,show un]

> rigActionSuccess :: IConnection conn => conn -> String -> Bool -> IO ()
> rigActionSuccess conn ovr sttng =
>   callSp conn "action_rig_action_success" [ovr, show sttng]

> addMagicSword :: IConnection conn => Database -> conn -> String -> IO ()
> addMagicSword db _ wiz = do
>   transaction db $
>     update db wizards
>       (\r -> r # wizard_name .==. constant wiz)
>       (\_ -> magic_sword .=. constant True .*. emptyRecord)
>   --runSql conn "update wizards set magic_sword = true where wizard_name = ?" [wiz]

> killTopPieceAt :: IConnection conn => conn -> Int -> Int -> IO ()
> killTopPieceAt conn xp yp =
>   callSp conn "kill_top_piece_at" [show xp, show yp]



> addSpell1 :: IConnection conn => conn -> String -> String -> IO ()
> addSpell1 conn wiz spellName = do
>   {-insert db Sb.spell_books r
>   where
>     r :: Sb.Spell_books
>     r = Sb.xid .=. constant 201 --_default
>         .*. Sb.wizard_name .=. constant wiz
>         .*. Sb.spell_name .=. constant spellName
>         .*. emptyRecord-}
>   _ <- run conn "insert into spell_books (spell_name, wizard_name)\n\
>          \values (?, ?);" [toSql spellName, toSql wiz]
>   commit conn

> {-sendKeyPress :: IConnection conn => conn -> String -> IO ()
> sendKeyPress conn k = do
>   callSp conn "action_key_pressed" [k]-}

> disableSpreading :: IConnection conn => Database -> conn -> IO ()
> disableSpreading db _ =
>   transaction db $
>     update db disable_spreading_table
>       (\_ -> constant True)
>       (\_ -> disable_spreading .=. constant True .*. emptyRecord)
>   --runSql conn "update disable_spreading_table\n\
>   --           \set disable_spreading = true;" []

> nextPhaseChooseIf :: IConnection conn => conn -> Bool -> IO ()
> nextPhaseChooseIf conn c = do
>   withTransaction conn $ \cn -> do
>     when c $ callSpNoCommit cn "action_choose_disbelieve_spell" []
>     callSpNoCommit cn "action_next_phase" []
>     --callSpNoCommit cn "action_key_pressed" ["space"]

> nextPhase :: IConnection conn => conn -> IO ()
> nextPhase conn = callSp conn "action_next_phase" []

> actionGo :: IConnection conn => Database -> conn -> IO ()
> actionGo _ conn = callSp conn "action_go" []

> data CursorDirection = CursorUp
>                      | CursorUpLeft
>                      | CursorLeft
>                      | CursorDownLeft
>                      | CursorDown
>                      | CursorDownRight
>                      | CursorRight
>                      | CursorUpRight

> actionMoveCursor :: IConnection conn => Database -> conn -> CursorDirection -> IO()
> actionMoveCursor _ conn cd = callSp conn "action_move_cursor" [c]
>                           where
>                             c = case cd of
>                                   CursorUp -> "up"
>                                   CursorUpLeft -> "up-left"
>                                   CursorLeft -> "left"
>                                   CursorDownLeft -> "down-left"
>                                   CursorDown -> "down"
>                                   CursorDownRight -> "down-right"
>                                   CursorRight -> "right"
>                                   CursorUpRight -> "up-right"



> skipToPhase :: IConnection conn => Database -> conn -> String -> IO ()
> skipToPhase _ conn phase =
>   callSp conn "action_skip_to_phase" [phase]

> rollback :: IConnection conn => conn -> IO ()
> rollback conn = run conn "rollback" [] >> return ()


> callSp :: IConnection conn => conn -> String -> [String] -> IO ()
> callSp conn spName args = do
>   callSpNoCommit conn spName args
>   commit conn

> callSpNoCommit :: IConnection conn => conn -> String -> [String] -> IO ()
> callSpNoCommit conn spName args = do
>   let qs = intersperse ',' $ replicate (length args) '?'
>       sqlString = "select " ++ spName ++ "(" ++ qs ++ ")"
>   _ <- run conn sqlString $ map toSql args
>   return ()


> {-disableConstraints :: IConnection conn => conn -> IO ()
> disableConstraints conn = callSp conn "disable_all_constraints" []

> enableConstraints :: IConnection conn => conn -> IO ()
> enableConstraints conn = callSp conn "enable_all_constraints" []-}

> withConstraintsDisabled :: IConnection conn => conn -> IO () -> IO ()
> withConstraintsDisabled conn f =
>   bracket (callSp conn "disable_all_constraints" [])
>           (const $ callSp conn "enable_all_constraints" [])
>           (const f)


> {-runSql :: IConnection conn => conn -> String -> [String] -> IO ()
> runSql conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn-}


> {-selectRelation :: (IConnection conn) =>
>                   conn -> String -> [SqlValue] -> IO [[SqlValue]]
> selectRelation conn q args = do
>   sth <- prepare conn q
>   _ <- execute sth args
>   v <- fetchAllRows' sth
>   return v-}
