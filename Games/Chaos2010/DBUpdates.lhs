> module Games.Chaos2010.DBUpdates
>     (resetNewGameWidgetState
>     ,newGame
>     ,sendKeyPress
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
>     --,disableConstraints
>     --,enableConstraints
>     ) where
> import Database.HDBC hiding (rollback)
> import Database.HaskellDB
> import Data.List (intersperse)
> --import Database.HaskellDB.Query
> import Control.Exception


> import Games.Chaos2010.Database.Wizards
> import qualified Games.Chaos2010.Database.New_game_widget_state as Ng
> import qualified Games.Chaos2010.Database.Cursor_position as C
> import qualified Games.Chaos2010.Database.Pieces as P
> import qualified Games.Chaos2010.Database.Disable_spreading_table as D

> setupTestBoard :: IConnection conn => conn -> String -> IO ()
> setupTestBoard conn t =
>     callSp conn "action_setup_test_board" [t]

> updateNewGameState :: IConnection conn => Database -> conn -> Int -> String -> IO ()
> updateNewGameState db conn l s =
>   transaction db $
>     update db Ng.new_game_widget_state
>       (\r -> r # Ng.line .==. constant l)
>       (\_ -> Ng.state .=. constant s .*. emptyRecord)

>   --  runSql conn "update new_game_widget_state\n\
>   --              \set state =? where line =?" [s, show l]


> resetNewGameWidgetState :: IConnection conn => conn -> IO ()
> resetNewGameWidgetState conn = callSp conn "action_reset_new_game_widget_state" []

> setAllHuman :: IConnection conn => Database -> conn -> IO ()
> setAllHuman db conn =
>   transaction db $
>     update db Ng.new_game_widget_state
>       (\_ -> constant True)
>       (\_ -> Ng.state .=. constant "human" .*. emptyRecord)
>   --runSql conn "update new_game_widget_state set state='human';" []

> newGame :: IConnection conn => conn -> IO ()
> newGame conn = callSp conn "action_client_new_game_using_new_game_widget_state" []

> setCursorPos :: IConnection conn => Database -> conn -> Int -> Int -> IO ()
> setCursorPos db conn xp yp =
>   transaction db $
>     update db C.cursor_position
>       (\_ -> constant True)
>       (\_ -> C.x .=. constant xp .*. C.y .=. constant yp .*. emptyRecord)
>   --runSql conn "update cursor_position set x=?, y=?" [show xp,show yp]

> killWizard :: IConnection conn => conn -> String -> IO ()
> killWizard conn wiz =
>       callSp conn "kill_wizard" [wiz]

> setWizardPosition :: IConnection conn => Database -> conn -> String -> Int -> Int -> IO ()
> setWizardPosition db conn allegiance xp yp =
>   transaction db $
>     update db P.pieces
>       (\r ->  (r # P.ptype .==. constant "wizard")
>              .&&. (r # P.allegiance .==. constant allegiance))
>       (\_ -> P.x .=. constant xp
>              .*. P.y .=. constant yp
>              .*. emptyRecord)
>            --runSql conn "update pieces set x = ?, y = ?\n\
>            --             \where ptype='wizard' and allegiance=?"
>            --        [show xp, show yp, allegiance]

> createCorpse :: IConnection conn => conn -> String -> Int -> Int -> Bool -> IO ()
> createCorpse conn ptype x y im =
>           callSp conn "create_corpse"
>                   [ptype,
>                    (show x),
>                    (show y),
>                    show im]

> createPieceInternal :: IConnection conn => conn -> String -> String -> Int -> Int -> Bool -> Bool -> IO ()
> createPieceInternal conn ptype allegiance x y im un = do
>     callSp conn "create_piece_internal"
>                   [ptype
>                   ,allegiance
>                   ,(show x)
>                   ,(show y)
>                   ,show im
>                   ,show un]

> rigActionSuccess :: IConnection conn => conn -> String -> Bool -> IO ()
> rigActionSuccess conn override setting =
>   callSp conn "action_rig_action_success" [override, show setting]

> addMagicSword :: IConnection conn => Database -> conn -> String -> IO ()
> addMagicSword db conn wiz = do
>   transaction db $
>     update db wizards
>       (\r -> r # wizard_name .==. constant wiz)
>       (\_ -> magic_sword .=. constant True .*. emptyRecord)
>   --runSql conn "update wizards set magic_sword = true where wizard_name = ?" [wiz]

> killTopPieceAt :: IConnection conn => conn -> Int -> Int -> IO ()
> killTopPieceAt conn x y =
>   callSp conn "kill_top_piece_at" [show x, show y]



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

> sendKeyPress :: IConnection conn => conn -> String -> IO ()
> sendKeyPress conn k = do
>   callSp conn "action_key_pressed" [k]

> disableSpreading :: IConnection conn => Database -> conn -> IO ()
> disableSpreading db conn =
>   transaction db $
>     update db D.disable_spreading_table
>       (\_ -> constant True)
>       (\_ -> D.disable_spreading .=. constant True .*. emptyRecord)
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
