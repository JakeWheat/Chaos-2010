
> {-# LANGUAGE EmptyDataDecls,DeriveDataTypeable #-}
> module Games.Chaos2010.Tests.Complete (complete) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)
> import Database.HaskellDB.Database
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.Utils
> import Games.Chaos2010.Database.Action_history_mr
> import Games.Chaos2010.Database.Client_valid_activate_actions
> import Games.Chaos2010.Database.Client_valid_target_actions

> complete :: IConnection conn => Database -> conn -> Test.Framework.Test
> complete db conn = testGroup "complete" $
>                   map (\xx -> xx db conn)
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
>   newSetupGame db conn (setPhase "move"
>                         . setCurrentWizard "Kong Fuzi") ("\n\
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
>   goSquare db conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 0 0
>   sendKeyPress conn "space"
>   assertCount "game won history entry" db
>                     (do
>                       t1 <- table action_history_mr
>                       restrict ((t1 .!. history_name) .==. constant "game_won")
>                       project $ copyAll t1) 1
>   assertCount "now valid target actions"
>                     db
>                     (table client_valid_target_actions)
>                     0
>   assertCount "now valid activate actions"
>                     db
>                     (table client_valid_activate_actions)
>                     0

> assertCount :: Database.HaskellDB.Database.GetRec er vr =>
>                String
>             -> Database
>             -> Query (Rel (Record er))
>             -> Int
>             -> IO ()
> assertCount m db t1 c = do
>     c1 <- getCount db t1
>     assertEqual m c c1

 > data CountTag deriving Typeable
 > type Count = Proxy CountTag

instance ShowLabel Count where showLabel _ = "count"

 > xcount :: Count
 > xcount = proxy


Draw works similarly to win, except it is detected the instant there
are no wizards left.

> testGameDraw :: IConnection conn => Database -> conn -> Test.Framework.Test
> testGameDraw db = tctor "testGameDraw" $ \conn -> do
>   newSetupGame db conn (setPhase "move"
>                         . setCurrentWizard "Kong Fuzi") ("\n\
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
>   goSquare db conn 1 0
>   rigActionSuccess conn "attack" True
>   goSquare db conn 0 0
>   --now magically kill the remaining to force the draw
>   killTopPieceAt conn 3 0
>   assertCount "game drawn history entry" db
>                     (do
>                       t1 <- table action_history_mr
>                       restrict ((t1 .!. history_name) .==. constant "game_drawn")
>                       project $ copyAll t1) 1
>   assertCount "now valid target actions"
>                     db
>                     (table client_valid_target_actions)
>                     0
>   assertCount "now valid activate actions"
>                     db
>                     (table client_valid_activate_actions)
>                     0
