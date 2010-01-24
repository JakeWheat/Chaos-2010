#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

= Overview
introduction
main
info widget
board widget
spell book widget
new game widget
action history widget
window manager widget
sprite manager - loads the pngs from disk
my text view - some additional functions for working with text views
utils


= Introduction

This is the code to hook the client pl/pgsql code up to gtk.

to produce an executable try:
ghc --make -o chaos chaos.lhs

== widget summary
Window_manager - lists windows
New game - view and change the options when starting a new game
Board - shows the board and the pieces on it
Info - shows info/stats on one or more pieces
spells - view spell list for wizard or entire spell list, details for
each spell, choose spell

== Error handling:

no thought has been put into the error handling, some bits of code try
to muddle through (bits that read from the database to display) and
others force the whole app to quit.

== control summary

This is half notes and half what has been coded.

escape to exit program

ctrl-space to continue, uses space at the moment but this is a bit
easy to hit

num pad and cursor keys to move cursor

choose phase:
  letter to select spell
  end to select no spell
cast phase
  num 5 or enter to cast/ select target
  end to cancel or finish spell
move
  num 5 or enter to
    select piece
    select flying target
    select ranged target
  end to unselect piece

mouse (TODO)
click continue to continue
click buttons in window manager to hide/show windows or close windows
click exit in window manager to exit/ close window manager window
choose:
  click spell to select
  click no spell to select done
cast
  click anywhere for non target spell
  click target for targeted spell
  right click to cancel
move
  click piece to select
  click square to move
  click enemy to attack
  click target to fly/ range attack
  right click to unselect


================================================================================

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad
> import System.Environment
> import Control.Concurrent

> import Games.Chaos2010.Dbms.ChaosDB
> import qualified Games.Chaos2010.Dbms.DBAdmin as DBAdmin
> import qualified Games.Chaos2010.Conf as Conf
> import Games.Chaos2010.Utils
> import Games.Chaos2010.Misc.FileAdmin
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.Misc.SetupUtils as SetupUtils

> import Games.Chaos2010.UI.SetupUI

================================================================================

= Main

Most of the bootstrapping is done in the windowManagerNew function

all that main needs to do is call this and call the two gtk init actions

> main :: IO ()
> main = do
>  Logging.setupLogging
>  conf <- SetupUtils.checkSetup
>  args <- getArgs
>  case True of
>    _ | (length args == 1 && head args == "reset") ->
>            time (DBAdmin.reset conf)
>      | (length args == 1 && head args == "switch") ->
>            time (DBAdmin.switchOverTempDb conf)
>      | (length args == 1 && head args == "setup") ->
>            time (DBAdmin.setup conf)
>      | (length args == 1 && head args == "checkSprites") ->
>            time checkSprites
>      | not (null args) ->
>            putStrLn "Call with no arguments to run game,\n\
>                     \or run 'chaos setup' to initialise database\n\n\
>                     \For developers: with exactly one of these \
>                     \arguments:\n\
>                     \reset\treset database from sql\n\
>                     \switch\tswitch temp db\n\
>                     \checkSprites\tcheck sprite pngs\n"
>      | otherwise -> do
>          lg "initGui" "" unsafeInitGUIForThreadedRTS
>          timeoutAddFull (yield >> return True)
>                         priorityDefaultIdle 50
>          withConn ("host=localhost dbname=" ++ Conf.dbName conf ++
>                    " user=" ++ Conf.username conf ++
>                    " password=" ++ Conf.password conf)
>            (\conn -> do
>              setupGui conn
>              mainGUI)

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)




Plan to get responsiveness fixed

Two main areas where the responsiveness is not good enough:

1) moving the cursor around

2) trying to click a button or quit whilst the ai is running

TODO: get some logging to highlight how long it takes to respond to
each event, also how long the gtk thread is updating instead of
waiting for the user - percentages, distribution of times.

Some ideas on how to fix these issues:

1) only have one update for each of the widgets running at one
time. If another update is needed and the current running update is
not in the render phase, kill it (need to be careful of problems
whilst the idle handler is being setup, and also check to see if
killing the update thread doesn't break an already setup idle handler

2) don't use this for the board widget, which must update as quickly
as possible. Never cancel a board widget update, but only ever have
one pending. To speed it up, kill and requeue any non board updates
running when an update board widget request is queued.

3) when anything changes, start the board widget update as soon as
possible, but wait 1/2 a second or something for the other widgets. if
another change comes in before the 1/2 second is up, reset it to wait
another 1/2 second - this might starve the other windows from being
updated though, maybe only do this if the update is a cursor move, or
maybe make them wait only if a board update is running, and kill and
requeue them if they are running during a board update. Need to test
to see what kind of latencies we get.
