
> module Games.Chaos2010.Tests.Complete (complete) where

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
> import Database.HDBC

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
>
> complete db conn = testGroup "complete" $
>                   map (\x -> x db conn)
>                       [testWizardWin
>                       ,testGameDraw
>                 ]

================================================================================

= winning/drawing

Win is tested when next phase is called following the original
chaos. We check that the win has been detected by seeing the win
history item in the history table, and by checking the valid activate
and target action views are empty.

> testWizardWin :: IConnection conn => Database -> conn -> Test.Framework.Test
> testWizardWin db = tctor "testWizardWin" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
>                   \1G     2       \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               ",
>                   (wizardPiecesList ++
>                   [('G', [makePD "goblin" "Kong Fuzi"])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   sendKeyPress conn "space"
>   undefined {-
>   assertRelvarValue "game won history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game_won';" []
>                     (1::Int)
>   assertRelvarValue "now valid target actions"
>                     conn "select count(1)\n\
>                          \from client_valid_target_actions;" []
>                     (0::Int)
>   assertRelvarValue "now valid activate actions"
>                     conn "select count(1)\n\
>                          \from client_valid_activate_actions;" []
>                     (0::Int) -}


Draw works similarly to win, except it is detected the instant there
are no wizards left.

> testGameDraw :: IConnection conn => Database -> conn -> Test.Framework.Test
> testGameDraw db = tctor "testGameDraw" $ \conn -> do
>   startNewGameReadyToMove db conn ("\n\
>                   \1G 2           \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               ",
>                   (wizardPiecesList ++
>                   [('G', [makePD "green_dragon" "Kong Fuzi"])]))
>   sendKeyPress conn "space"
>   goSquare conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare conn 0 0
>   --now magically kill the remaining to force the draw
>   runSql conn "select kill_top_piece_at(?,?);" ["3","0"]
>   undefined {-
>   assertRelvarValue "game drawn history entry"
>                     conn "select count(1) from action_history_mr\n\
>                          \where history_name='game_drawn';" []
>                     (1::Int)
>   assertRelvarValue "now valid target actions"
>                     conn "select count(1)\n\
>                          \from client_valid_target_actions;" []
>                     (0::Int)
>   assertRelvarValue "now valid activate actions"
>                     conn "select count(1)\n\
>                          \from client_valid_activate_actions;" []
>                     (0::Int) -}

