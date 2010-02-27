
> {-# LANGUAGE ScopedTypeVariables #-}
> module Games.Chaos2010.Tests.Basics (basics) where

> import Test.Framework
> import Control.Monad

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.DBUpdates
> import Games.Chaos2010.Tests.SetupGameState


> basics :: IConnection conn => Database -> conn -> Test.Framework.Test
> basics db conn = testGroup "basics" [
>                   --testDatabaseStuff conn
>                   testCursorMovement db conn
>                  ,testPiecesOnTop db conn
>                  ]

== pieces on top

check our pieces on top logic.

Refresher:
corpses cannot be interacted with if there is another piece
(you can interact with corpses by disbelieve, raise dead, also
 possibly ranged attack, bolt, vengeance...?)
wizards can't be interacted with when they are in a castle or magic tree
or riding a monster
monsters cannot be interacted with if there is a gooey blob on the square
here is a list of the combos, with the interactable piece last:
a corpse monster
b corpse wizard
c corpse wizard monster
d wizard monster
e wizard castle
f wizard magic tree
g corpse gooey
h monster gooey
i corpse monster gooey

TODO: we can usually only interact with the top piece on each square,
but there are exceptions: mounted wizard who hasn't moved, wizard in
magic tree or castle; add test for these.

> testPiecesOnTop :: IConnection conn => Database -> conn -> Test.Framework.Test
> testPiecesOnTop db = tctor "testPiecesOnTop" $ \conn -> do
>   setupGame db conn [useBoard ("\n\
>                   \b      c      d\n\
>                   \               \n\
>                   \ aghi          \n\
>                   \               \n\
>                   \e             f\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++ liftPl
>                    [('a', [("goblin", "Kong Fuzi")
>                           ,("elf", "dead")])
>                    ,('b', [("goblin", "dead")
>                           ,("wizard", "Buddha")])
>                    ,('c', [("pegasus", "Kong Fuzi")
>                           ,("giant", "dead")
>                           ,("wizard", "Kong Fuzi")])
>                    ,('d', [("gryphon", "Laozi")
>                           ,("wizard", "Laozi")])
>                    ,('e', [("magic_castle", "Moshe")
>                           ,("wizard", "Moshe")])
>                    ,('f', [("magic_tree", "Muhammad")
>                           ,("wizard", "Muhammad")])
>                    ,('g', [("goblin", "dead")
>                           ,("gooey_blob", "Buddha")])
>                    ,('h', [("goblin", "Kong Fuzi")
>                           ,("gooey_blob", "Buddha")])
>                    ,('i', [("goblin", "Kong Fuzi")
>                           ,("gooey_blob", "Buddha")
>                           ,("elf", "dead")])]))]
>   assertPiecesOnTopEquals db ("\n\
>                   \b      c      d\n\
>                   \               \n\
>                   \ aghi          \n\
>                   \               \n\
>                   \e             f\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++ liftPl
>                    [('a', [("goblin","Kong Fuzi")]),
>                     ('b', [("wizard","Buddha")]),
>                     ('c', [("pegasus","Kong Fuzi")]),
>                     ('d', [("gryphon","Laozi")]),
>                     ('e', [("magic_castle","Moshe")]),
>                     ('f', [("magic_tree","Muhammad")]),
>                     ('g', [("gooey_blob","Buddha")]),
>                     ('h', [("gooey_blob","Buddha")]),
>                     ('i', [("gooey_blob","Buddha")])]))

== cursor movement

Check the cursor movement and also the shortcut for the tests to move
the cursor to a given position, also check the moveto code

> testCursorMovement :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCursorMovement db = tctor "testCursorMovement" $ \conn -> do
>   --make sure there is a game running:
>   setupGame db conn []
>   let moveAndCheck m xp yp = do
>         actionMoveCursor db conn m
>         assertCursorPositionEquals db xp yp
>   --move to our starting position then move in a circle

>   moveAndCheck CursorDownRight 1 1
>   moveAndCheck CursorDownRight 2 2
>   moveAndCheck CursorDownRight 3 3
>   moveAndCheck CursorRight 4 3
>   moveAndCheck CursorUpRight 5 2
>   moveAndCheck CursorUp 5 1
>   moveAndCheck CursorUpLeft 4 0
>   moveAndCheck CursorLeft 3 0
>   moveAndCheck CursorDownLeft 2 1
>   moveAndCheck CursorDown 2 2
>   --now test the move cursor to straight up, left, down, right,
>   let moveToAndCheck xp yp = do
>           moveCursorToK db conn xp yp
>           assertCursorPositionEquals db xp yp
>   moveToAndCheck 5 5
>   moveToAndCheck 5 1
>   moveToAndCheck 5 6
>   moveToAndCheck 3 6
>   moveToAndCheck 10 6
>   --upleft downleft  upright downright
>   moveToAndCheck 7 3
>   moveToAndCheck 4 6
>   moveToAndCheck 8 4
>   moveToAndCheck 12 8
>   --corners
>   moveToAndCheck 0 0
>   moveToAndCheck 14 0
>   moveToAndCheck 0 9
>   moveToAndCheck 14 9
>
>   --some random places around the board
>   --todo
>
>   return ()

start with a few helper functions

get the current cursor position from the database

move the cursor to x,y, using key presses

> moveCursorToK :: IConnection conn => Database -> conn -> Int -> Int -> IO ()
> moveCursorToK db conn xp yp = do
>     --diagonals first then straight moves
>     (cx,cy) <- queryCursorPosition db
>     --putStrLn $ "move " ++ (show (cx,cy)) ++ " to " ++ (show (x,y))
>     unless ((cx,cy) == (xp,yp)) $ do
>         let (dx,dy) = (xp - cx, yp - cy)
>             diagonalMoves = minimum [abs dx, abs dy]
>             diagonalDirection = case True of
>                                 _ | dx == 0 && dy == 0 -> CursorUpLeft
>                                   | dx < 0 && dy < 0 -> CursorUpLeft
>                                   | dx < 0 && dy > 0 -> CursorDownLeft
>                                   | dx > 0 && dy < 0 -> CursorUpRight
>                                   | dx > 0 && dy > 0 -> CursorDownRight
>                                   | otherwise -> error
>                                        "pattern match: something \
>                                        \wrong in moveCursorTo"
>         -- do it in two stages cos I'm not smart enough
>         sequence_ (replicate diagonalMoves
>                    (actionMoveCursor db conn diagonalDirection))
>         (ncx,ncy) <- queryCursorPosition db
>         unless ((cx,cy) == (xp,yp)) $ do
>           let (dir,amount) = case True of
>                            _ | ncx < xp -> (CursorRight, xp - ncx)
>                              | ncx > xp -> (CursorLeft, ncx - xp)
>                              | ncy < yp -> (CursorDown, yp - ncy)
>                              | ncy > yp -> (CursorUp, ncy - yp)
>                              | otherwise -> (CursorUp, 0)
>           sequence_ (replicate amount
>                        (actionMoveCursor db conn dir))
