> module Games.Chaos2010.Tests.Phases (phases) where

> import Test.Framework
> import Control.Monad

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Utils
> import Games.Chaos2010.Tests.SetupGameState

> phases :: IConnection conn => Database -> conn -> Test.Framework.Test
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
>   testPhases db conn wizardNames

> testPhases :: IConnection conn =>
>               Database -> conn -> [String] -> IO ()
> testPhases db conn wizs = do
>   setupGame db conn $ [setWizards wizs]
>   forM_ ["choose","cast","move","choose","cast","move"] $ \phase ->
>     forM_ wizs $ \wiz -> do
>       assertCurrentWizardPhase db wiz phase
>       nextPhaseChooseIf conn (phase == "choose")


test next phase with some wizards not choosing spells

now test it works with one or more wizards dead:
start at choose on first wizard and run though twice
to do all variations is 256 tests

> testNextPhaseWizardDead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhaseWizardDead db = tctor "testNextPhaseWizardDead" $ \conn ->
>   forM_ [0..7] $ \j -> do
>     testPhases db conn $ dropItemN wizardNames j

> testNextPhaseTwoWizardsDead :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNextPhaseTwoWizardsDead db = tctor "testNextPhaseTwoWizardsDead" $ \conn ->
>   forM_ [0..7] $ \j -> forM_ [(j + 1)..7] $ \k -> do
>       testPhases db conn $ dropItemN (dropItemN wizardNames k) j


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
