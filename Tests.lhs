#!/usr/bin/env runghc

Copyright 2009 Jake Wheat

= Overview

main - main function, which runs the tests

tests - basic tests
spell cast tests
move actions - tests for the actions in the move phase
autonomous - tests for actions in the autonomous phase

diagrams - ascii art type thing to help readability of tests
test helpers
setup game functions - used to set up a game before running a test

db update helpers
db read shortcuts

= Introduction

Most of the tests revolve around setting up a game, then running a few
actions and checking what happens in the database. The actions mostly
run by feeding letters to the handle key press action to simulate
someone actually using the ui, the main big hole in the tests is
checking the displayed data in the ui.

We try to get maximum effect for a minimal amount of tests, so there
is no great attempt at comprehensiveness.

In order to speed the run of tests up, some items which might live in
different tests are combined to reduce the amount of work needed,
e.g. testing that next_phase is automatically called when a wizard
finished casting his spell is combined with a test for casting a
spell. This is probably evil and wrong.

Speed: running all the tests takes ages for very little work, this
highlights how slow the code is. I'm waiting for postgresql 8.4 which
apparently has easy function profiling to find out why it's so
slow. Probably some materialised views are needed.

Current Todo planned for beta:
create aliases for the keypress calls
move subphase tests
move misc tests
line of sight
prompt
autonomous
loop through all spells
missing wizard upgrades
valid squares after all updates

== Notes for checking database updates

Want to check all the data tables after each action is run, so we keep
track of all the relvar updates and make sure all the relvars which
shouldn't change haven't changed.

data relvars plus notes

history: added to each action
prompt: changes depending on valid squares
alignment: may change after casting
pieces: usually checked directly using the diagram stuff
current wizard
game completed
pieces to move
selected piece
spell parts to cast
squares left to walk
turn number
turn phase
wizard spell choices
spell books
wizards
cast success checked
cursor pos - tested implicitly all the time

data relvars that are always empty outside of a transactions
(metaphorically speaking these would be put on the stack instead of
the heap if we could):

cast magic wood squares
cast alignment
creating new game
*_hack

data relvars to skip testing
piece starting ticks
board size
new game widget state
spell books show all
test action overrides
wizard display info
windows

reduce testing work:

don't check relvars which are unchanged every action, just check their
value immediately after they've changed ore at start of test, and
check they are still the same immediately before they're changed or at
end of test.

Don't check data covered by database constraints (at some point want
to find a way of testing the database constraints themselves).

> {-# LANGUAGE ScopedTypeVariables #-}
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
> import Games.Chaos2010.Tests.AllTests
> import Games.Chaos2010.Tests.TestUtils

> --import ChaosDB
> --import Games.Chaos2010.Conf
> --import Games.Chaos2010.Utils
> --import qualified Games.Chaos2010.Misc.Logging as Logging
> --import Games.Chaos2010.Database
> --import Games.Chaos2010.Database.Pieces_mr



================================================================================

= Main

Run all the tests.

> main :: IO ()
> main = do -- time $ do
>   -- Logging.setupLogging
>   -- conf <- getConfig
>   postgresqlConnect [("dbname", "chaos")] $ \db -> do
>   withConn ("dbname=chaos") $ \conn -> do
>   resetNewGameWidgetState conn
>   setAllHuman conn
>   defaultMain $ allTests db conn
>   return ()

> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (connectPostgreSQL cs)
>                         disconnect
>                         f


postgresql.conf  #track_functions = none # none, pl, all

 select * from pg_stat_user_functions ;
http://www.depesz.com/index.php/2008/05/15/waiting-for-84-function-stats/

> {-testHaskellDB :: Database -> IO ()
> testHaskellDB db = do
>   putStrLn "here"
>   {-let q = do u <- table pieces
>           restrict ((u!name .==. user) .&&.(u!password .==. passwd))
>           project (rights << u!rights) -}
>   res <- query db $ table pieces_mr --q
>   mapM_ putStrLn $ map convRow res
>   return ()
>   where
>     --convRow :: Record a -> String
>     convRow r = show $ r # ptype-}

undefined


================================================================================

= Tests

== tests in sql

First run the tests from the database, these are all the database
functions whose name starts with 'check_code_'

> {-testDatabaseStuff :: Connection -> Test.Framework.Test
> testDatabaseStuff db = tctor "testDatabaseStuff" $ \conn -> do
>   res <- selectTuplesIO conn "select object_name\n\
>                     \from module_objects\n\
>                     \where object_name ~ 'check_code_.*'\n\
>                     \and object_type='operator';" []
>                    (\t -> do
>      v <- selectValue conn ("select " ++  lk "object_name" t ++ "()") []
>      return (lk "object_name" t, read v::Bool))
>   let notpass = filter (\(_,b) -> not b) res

>   when (length notpass > 0)
>      (assertBool ("sql tests failed: " ++
>                   intercalate "\n" (map fst notpass)) False)

>   return ()-}






--------------------------------------------------------------

Todo ideas for tests:

? Write tests for the more complicated logic in the sql.  e.g. some of
the constraint system, etc.

Fix up tests already written:

Add test fragments to run at every stage to check all the available
actions and all their valid arguments.

check all changes to the database after each action and also check no
other changes are made.

Fix probabilities:

write tests to run tests many times and check the probabilities

for all the actions which involve a piece moving square write a test
to cover all options e.g. test walk attack in all eight directions

make the tests hierarchical, use a better tool to run them

Make the tests more watertight: at least one test for each spell,
mirror tests for each wizard

Add variants for all actions which can be repeated: e.g. walk 3 squares
cast 4 spells.

Check any actions which don't have tests and write them.

Use a quickcheck style spec to generate more exhaustive tests?

some other todos:

try to select and do stuff that isn't allowed at a particular time,
make sure nothing happens, looking to prevent the game crashing with a
database constraint fail

find some way to record a full game and replay it as a test to check
everything

run each action from each wizard, and from a monster and a wizard variations
when applicable

