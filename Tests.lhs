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
> import Test.Framework
> import Control.Exception

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HDBC.PostgreSQL
> import Database.HDBC

> import Games.Chaos2010.Tests.AllTests

================================================================================

= Main

Run all the tests.

> main :: IO ()
> main = do
>   postgresqlConnect [("dbname", "chaos")] $ \db -> do
>   withConn ("dbname=chaos") $ \conn -> do
>   _ <- run conn "update pg_settings\n\
>                 \  set setting=true\n\
>                 \  where name='log_duration'" []
>   _ <- run conn "update pg_settings\n\
>                 \  set setting='all'\n\
>                 \  where name='log_statement'" []
>   _ <- run conn "update pg_settings\n\
>                 \  set setting='0'\n\
>                 \  where name='log_min_duration_statement'" []
>   commit conn
>   defaultMain $ allTests db conn
>   return ()

> withConn :: String -> (Connection -> IO c) -> IO c
> withConn cs f = bracket (connectPostgreSQL cs)
>                         disconnect
>                         f



update pg_settings
 set setting=true
  where name='log_duration';
update pg_settings
  set setting='all'
  where name='log_statement';
update pg_settings
  set setting='0'
  where name='log_min_duration_statement';