
> module Games.Chaos2010.Tests.Basics (basics) where

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
>
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

> testPiecesOnTop :: Database -> Connection -> Test.Framework.Test
> testPiecesOnTop db = tctor "testPiecesOnTop" $ \conn -> do
>   startNewGame conn
>   setupBoard conn ("\n\
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
>                           ,("elf", "dead")])]))
>   assertTopPiecesEquals db ("\n\
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

> testCursorMovement :: Database -> Connection -> Test.Framework.Test
> testCursorMovement db = tctor "testCursorMovement" $ \conn -> do
>   --make sure there is a game running:
>   startNewGame conn
>   --reset the cursor position
>   runSql conn "update cursor_position set x=0,y=0;" []
>   let moveAndCheck m x y = do
>         sendKeyPress conn $ cursorShorthand m
>         checkCursorPosition conn x y
>   --move to our starting position then move in a circle

>   moveAndCheck "dr" 1 1
>   moveAndCheck "dr" 2 2
>   moveAndCheck "dr" 3 3
>   moveAndCheck "r" 4 3
>   moveAndCheck "ur" 5 2
>   moveAndCheck "u" 5 1
>   moveAndCheck "ul" 4 0
>   moveAndCheck "l" 3 0
>   moveAndCheck "dl" 2 1
>   moveAndCheck "d" 2 2
>   --now test the move cursor to straight up, left, down, right,
>   let moveToAndCheck x y = do
>           moveCursorToK conn x y
>           checkCursorPosition conn x y
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

avoid writing out the full key press names:


> cursorShorthand :: String -> String
> cursorShorthand m = undefined {-safeLookup "cursor shorthand" m
>                      [("d", "Down"),
>                       ("l", "Left"),
>                       ("u", "Up"),
>                       ("r", "Right"),
>                       ("dl", "KP_End"),
>                       ("dr", "KP_Page_Down"),
>                       ("ul", "KP_Home"),
>                       ("ur", "KP_Page_Up")]-}

get the current cursor position from the database

> readCursorPosition :: Connection -> IO (Int,Int)
> readCursorPosition conn = do
>   undefined
>   --r <- selectTuple conn "select x,y from cursor_position" []
>   --return (read $ lk "x" r, read $ lk "y" r)

move the cursor to x,y, using key presses

> moveCursorToK :: Connection -> Int -> Int -> IO ()
> moveCursorToK conn x y = do
>     --diagonals first then straight moves
>     (cx,cy) <- readCursorPosition conn
>     -- putStrLn $ "move " ++ (show (cx,cy)) ++ " to " ++ (show (x,y))
>     unless ((cx,cy) == (x,y)) $ do
>         let (dx,dy) = (x - cx, y - cy)
>             diagonalMoves = minimum [abs dx, abs dy]
>             diagonalDirection = case True of
>                                 _ | dx == 0 && dy == 0 -> "ul"
>                                   | dx < 0 && dy < 0 -> "ul"
>                                   | dx < 0 && dy > 0 -> "dl"
>                                   | dx > 0 && dy < 0 -> "ur"
>                                   | dx > 0 && dy > 0 -> "dr"
>                                   | otherwise -> error
>                                        "pattern match: something \
>                                        \wrong in moveCursorTo"
>         -- do it in two stages cos I'm not smart enough
>         sequence_ (replicate diagonalMoves
>                    (sendKeyPress conn $ cursorShorthand diagonalDirection))
>         (ncx,ncy) <- readCursorPosition conn
>         unless ((cx,cy) == (x,y)) $ do
>           let (dir,amount) = case True of
>                            _ | ncx < x -> ("r", x - ncx)
>                              | ncx > x -> ("l", ncx - x)
>                              | ncy < y -> ("d", y - ncy)
>                              | ncy > y -> ("u", ncy - y)
>                              | otherwise -> ("u", 0)
>           sequence_ (replicate amount
>                        (sendKeyPress conn $ cursorShorthand dir))

> checkCursorPosition :: Connection
>                     -> Int
>                     -> Int
>                     -> IO ()
> checkCursorPosition conn x y = do
>   cp <- readCursorPosition conn
>   assertEqual "cursor position" cp (x, y)

