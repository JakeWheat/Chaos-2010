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
>     ,addSpell
>     ,rigActionSuccess
>     ,disableSpreading
>     ,addMagicSword
>     ,killTopPieceAt
>     ,setupTestBoard
>     ) where
> import Database.HDBC hiding (rollback)
> import Data.List

> setupTestBoard :: IConnection conn => conn -> String -> IO ()
> setupTestBoard conn t =
>     callSp conn "select action_setup_test_board(?);" [t]

> updateNewGameState :: IConnection conn => conn -> Int -> String -> IO ()
> updateNewGameState conn l s =
>     runSql conn "update new_game_widget_state\n\
>                 \set state =? where line =?" [s, show l]


> resetNewGameWidgetState :: IConnection conn => conn -> IO ()
> resetNewGameWidgetState conn = callSp conn "action_reset_new_game_widget_state" []

> setAllHuman :: IConnection conn => conn -> IO ()
> setAllHuman conn = runSql conn "update new_game_widget_state set state='human';" []

> newGame :: IConnection conn => conn -> IO ()
> newGame conn = callSp conn "action_client_new_game_using_new_game_widget_state" []

> setCursorPos :: IConnection conn => conn -> Int -> Int -> IO ()
> setCursorPos conn x y =
>   runSql conn "update cursor_position set x=?, y=?" [show x,show y]

> killWizard :: IConnection conn => conn -> String -> IO ()
> killWizard conn wiz =
>       callSp conn "kill_wizard" [wiz]

> setWizardPosition :: IConnection conn => conn -> String -> Int -> Int -> IO ()
> setWizardPosition conn allegiance x y =
>            runSql conn "update pieces set x = ?, y = ?\n\
>                         \where ptype='wizard' and allegiance=?"
>                    [show x, show y, allegiance]

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

> addMagicSword :: IConnection conn => conn -> String -> IO ()
> addMagicSword conn wiz =
>   runSql conn "update wizards set magic_sword = true\n\
>               \where wizard_name=?" [wiz]

> killTopPieceAt :: IConnection conn => conn -> Int -> Int -> IO ()
> killTopPieceAt conn x y =
>   runSql conn "select kill_top_piece_at(?,?);" [show x, show y]



> addSpell :: IConnection conn => conn -> String -> String -> IO ()
> addSpell conn wiz spellName =
>   runSql conn "insert into spell_books (spell_name, wizard_name)\n\
>          \values (?, ?);" [spellName, wiz]

> sendKeyPress :: IConnection conn => conn -> String -> IO ()
> sendKeyPress conn k = do
>   callSp conn "action_key_pressed" [k]

> disableSpreading :: IConnection conn => conn -> IO ()
> disableSpreading conn =
>   runSql conn "update disable_spreading_table\n\
>               \set disable_spreading = true;" []



> rollback :: IConnection conn => conn -> IO ()
> rollback conn = runSql conn "rollback" []


> callSp :: IConnection conn => conn -> String -> [String] -> IO ()
> callSp conn spName args = do
>   let qs = intersperse ',' $ replicate (length args) '?'
>       sqlString = "select " ++ spName ++ "(" ++ qs ++ ")"
>   _ <- run conn sqlString $ map toSql args
>   commit conn

> runSql :: IConnection conn => conn -> String -> [String] -> IO ()
> runSql conn sql args = do
>   _ <- run conn sql $ map toSql args
>   commit conn
