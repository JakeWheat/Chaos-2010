> module Games.Chaos2010.Tests.Phases (phases) where

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
> import Database.HDBC.PostgreSQL
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

> testNextPhase :: Database -> Connection -> Test.Framework.Test
> testNextPhase db = tctor "testNextPhase" $ \conn -> do
>   startNewGame conn
>   forM_ ["choose","cast","move","choose","cast","move"]
>         (\phase ->
>              forM_ [0..7] (\i -> do
>                checkCurrentWizardPhase conn (wizardNames !! i) phase
>                --so we don't skip the cast phase, make sure
>                -- each wizard has a spell chosen, use disbelieve
>                --cos wizards always have this spell available
>                undefined
>                {-whenA1 (readTurnPhase conn)
>                       (=="choose")
>                       (sendKeyPress conn "Q")-}
>                sendKeyPress conn "space"))

test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead :: Database -> Connection -> Test.Framework.Test
> testNextPhaseWizardDead db = tctor "testNextPhaseWizardDead" $ \conn ->
>   forM_ [0..7] (\j -> do
>     startNewGame conn
>     --kill wizard
>     callSp conn "kill_wizard" [wizardNames !! j]
>     let theseWizards = undefined -- dropItemN wizardNames j
>     forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>       forM_ [0..6] (\i -> do
>         checkCurrentWizardPhase conn (theseWizards !! i) phase
>         undefined {-whenA1 (readTurnPhase conn)
>                (=="choose")
>                (sendKeyPress conn "Q")-}
>         sendKeyPress conn "space")))

> testNextPhaseTwoWizardsDead :: Database -> Connection -> Test.Framework.Test
> testNextPhaseTwoWizardsDead db = tctor "testNextPhaseTwoWizardsDead" $ \conn ->
>   forM_ [0..7] (\j ->
>     forM_ [(j + 1)..7] (\k -> do
>       startNewGame conn
>       --kill wizards
>       callSp conn "kill_wizard" [wizardNames !! j]
>       callSp conn "kill_wizard" [wizardNames !! k]
>       let theseWizards = undefined {-dropItemN (dropItemN wizardNames k) j-}
>       forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>         forM_ [0..5] (\i -> do
>           checkCurrentWizardPhase conn (theseWizards !! i) phase
>           --so we don't skip the cast phase, make sure
>           -- each wizard has a spell chosen, use disbelieve
>           --cos wizards always have this spell available
>           undefined
>           {-whenA1 (readTurnPhase conn)
>                  (=="choose")
>                  (sendKeyPress conn "Q")-}
>           sendKeyPress conn "space"))))


check wizards dying during move when it is their turn - this can
happen if you shoot your own wizard with a ranged weapon from a
monster, the game should cope with it - I think this is tested in the
game drawn test

automatic next phase tests:
casting the last part of a spell moves to the next player automatically
moving the last creature moves to the next player automatically
these are tested in the spell cast and move sections respectively

> {-testNextPhaseAI :: Database -> Connection -> Test.Framework.Test
> testNextPhaseAI db = tctor "testNextPhaseAI" $ \conn -> do
>   startNewGameAI conn
>   forM_ ["choose","cast","move","choose","cast","move"] (\phase ->
>     forM_ [0..7] (\i -> do
>        checkCurrentWizardPhase conn (wizardNames !! i) phase
>        sendKeyPress conn "space"))-}


