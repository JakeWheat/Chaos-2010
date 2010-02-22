> module Games.Chaos2010.Tests.Phases (phases) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.List
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

> phases db conn = testGroup "phases" $
>                  map (\x -> x db conn)
>                  [testNextPhase
>                  ,testNextPhaseWizardDead
>                  ,testNextPhaseTwoWizardsDead
>                   --,testNextPhaseAI
>                  ]


== next phase

Just run through the choose, cast and move phases for each wizard
twice, check the turn_phase and current_wizard each time

> testNextPhase :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhase db = tctor "testNextPhase" $ \conn -> do
>   startNewGame conn
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                assertCurrentWizardPhase db (wizardNames !! i) phase
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                whenA1 (queryTurnPhase db)
>                       (=="choose")
>                       (sendKeyPress conn "Q")
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhaseWizardDead db = tctor "testNextPhaseWizardDead" $ \conn ->
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizardNames !! j]
>     let theseWizards = dropItemN wizardNames j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>         assertCurrentWizardPhase db (theseWizards !! i) phase
>         whenA1 (queryTurnPhase db)
>                (=="choose")
>                (sendKeyPress conn "Q")
>         sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhaseTwoWizardsDead db = tctor "testNextPhaseTwoWizardsDead" $ \conn ->
>   forM_ [0..7] (\j ->
>     forM_ [(j + 1)..7] (\k -> do
>       startNewGame conn
>       --kill wizards
>       callSp conn "kill_wizard" [wizardNames !! j]
>       callSp conn "kill_wizard" [wizardNames !! k]
>       let theseWizards = dropItemN (dropItemN wizardNames k) j
>       forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>         forM_ [0..5] (\i -> do
>           assertCurrentWizardPhase db (theseWizards !! i) phase
>           --so we don't skip the cast phase, make sure
>           -- each wizard has a spell chosen, use disbelieve
>           --cos wizards always have this spell available
>           whenA1 (queryTurnPhase db)
>                  (=="choose")
>                  (sendKeyPress conn "Q")
>           sendKeyPress conn "space"))))


check wizards dying during move when it is their turn - this can
happen if you shoot your own wizard with a ranged weapon from a
monster, the game should cope with it - I think this is tested in the
game drawn test

automatic next phase tests:
casting the last part of a spell moves to the next player automatically
moving the last creature moves to the next player automatically
these are tested in the spell cast and move sections respectively

> {-testNextPhaseAI :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhaseAI db = tctor "testNextPhaseAI" $ \conn -> do
>   startNewGameAI conn
>   forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>     forM_ [0..7] (\i -> do
>        assertCurrentWizardPhase db (wizardNames !! i) phase
>        sendKeyPress conn "space"))-}



> whenA1 :: IO a -> (a -> Bool) -> IO () -> IO ()
> whenA1 feed cond f = (cond `liftM` feed) >>= flip when f

> dropItemN :: [a] -> Int -> [a]
> dropItemN [] _ = []
> dropItemN (x:xs) i = if i == 0
>                        then xs
>                        else x: dropItemN xs (i - 1)
