
Tests for the squares valid logic

stage 1:
test the square categories.

the square categories is a view
category (text), x, y, maybe allegiance
the basic categories are: empty, top_attackable, top_creature,
top_monster, corpse only, not adjacent to tree

tests:
empty
top_attackable - single piece, non attackable piece, attackable &
corpse, wizard & mounted (?+corpse), wizard in box, gooey blob &
corpse, monster and gooey blob, corpse&monster&blob
top_creature, monster: -> same as top attackable
corpse only: same as top attackable - make sure the corpses with other
pieces aren't visible
not adjacent to tree: plonk some trees down and test

-> we can use one board to test all these

1 item
any
2 items
creature, stiff : creature on top
wizard, mountable monster: mountable monster on top
wizard, box : box on top
stiff, gooey blob : blob on top
monster, gooey blob : blob on top
3 items
wizard, stiff, mountable monster : mountable on top
stiff, monster, blob : blob on top

attackable needs undead field as well - >split


> module Games.Chaos2010.Tests.SquaresValid (squaresValid) where

> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.HaskellDBUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Tests.SetupGameState

> import Games.Chaos2010.Database.Squares_valid_categories
> import Games.Chaos2010.Database.Fields
> import Games.Chaos2010.Tests.RelationalAlgebra

> squaresValid :: IConnection conn => Database -> conn -> Test.Framework.Test
> squaresValid db conn = testGroup "squaresValid" [
>                   testSquareCategories db conn
>                  ]

> testSquareCategories :: IConnection conn => Database -> conn -> Test.Framework.Test
> testSquareCategories db = tctor "testSquareCategories" $ \conn -> do
>     setupGame db conn [setPhase "move"
>                       ,useBoard ("\n\
>                   \   t      t    \n\
>                   \ GCh m nDgHI   \n\
>                   \ ct   t    t   \n\
>                   \    t          \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",
>                   (wizardPiecesList ++
>                   [('G', [makePD "horse" "Buddha"])
>                   ,('C', [makePD "magic_castle" "Buddha"])
>                   ,('h', [makePD "goblin" "Buddha"
>                          ,makePD "goblin" "dead"])
>                   ,('m', [makePD "wizard" "Buddha"
>                          ,makePD "horse" "Buddha"])
>                   ,('n', [makePD "wizard" "Kong Fuzi"
>                          ,makePD "horse" "Kong Fuzi"
>                          ,makePD "goblin" "dead"])
>                   ,('D', [makePD "wizard" "Laozi"
>                          ,makePD "magic_castle" "Laozi"])
>                   ,('g', [makePD "goblin" "Buddha"
>                          ,makePD "gooey_blob" "Laozi"])
>                   ,('H', [makePD "goblin" "dead"
>                          ,makePD "gooey_blob" "Laozi"])
>                   ,('I', [makePD "goblin" "dead"
>                          ,makePD "goblin" "Buddha"
>                          ,makePD "gooey_blob" "Laozi"])
>                   ,('c', [makePD "goblin" "dead"])
>                   ,('t', [makePD "magic_tree" "Laozi"])
>                   ]))]
>     let emptySquares =
>           extend (const $ category .=. "empty")
>             $ parseValidSquares "\n\
>                   \XXX XXXXXX XXXX\n\
>                   \X   X X     XXX\n\
>                   \X  XXX XXXX XXX\n\
>                   \XXXX XXXXXXXXXX\n\
>                   \4XXXXXXXXXXXXX5\n\
>                   \XXXXXXXXXXXXXXX\n\
>                   \XXXXXXXXXXXXXXX\n\
>                   \XXXXXXXXXXXXXXX\n\
>                   \XXXXXXXXXXXXXXX\n\
>                   \6XXXXXX7XXXXXX8"
>         attackableSquares =
>           extend (const $ category .=. "attackable")
>             $ parseValidSquares "\n\
>                   \   X      X    \n\
>                   \ X X X X XXX   \n\
>                   \ XX   X    X   \n\
>                   \    X          \n\
>                   \X             X\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \X      X      X"
>         corpseOnlySquares =
>           extend (const $ category .=. "corpse-only")
>             $ parseValidSquares "\n\
>                   \               \n\
>                   \               \n\
>                   \ X             \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8"
>         treeAdjacentSquares =
>           extend (const $ category .=. "tree-adjacent")
>             $ parseValidSquares "\n\
>                   \  XXX    XXX   \n\
>                   \ XXXXXXX XXXX  \n\
>                   \ XXXXXXX  XXX  \n\
>                   \ XXXXXXX  XXX  \n\
>                   \   XXX         \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               "
>         wizardSquares =
>           extend (const $ category .=. "wizard")
>             $ parseValidSquares "\n\
>                   \               \n\
>                   \     X XX      \n\
>                   \               \n\
>                   \               \n\
>                   \X             X\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \X      X      X"
>         mountEnterSquares =
>           extend (const $ category .=. "mount-enter")
>             $ parseValidSquares "\n\
>                   \   X      X    \n\
>                   \ XX            \n\
>                   \  X   X    X   \n\
>                   \    X          \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8"
>     let vs = do
>              t1 <- table squares_valid_categories
>              project $ category .=. fn "" (t1 # category)
>                        .*. x .=. fn (-1) (t1 # x)
>                        .*. y .=. fn (-1) (t1 # y)
>                        .*. emptyRecord
>     assertRelvarValue db vs (emptySquares
>                              ++ attackableSquares
>                              ++ corpseOnlySquares
>                              ++ treeAdjacentSquares
>                              ++ wizardSquares
>                              ++ mountEnterSquares)

stage 2:
test valid squares for spells - want to test square contents and range
empty castle
empty or corspe only monster
attackable magic_bolt
vengeance creature on top
subversion monstr on top
corpse only raise dead
nottreeadj magic wood


stage 3
test the corner/special cases for other actions:
select hidden under blob
wizard on mount selection
mount/enter: occupied - castle, tree; wrong allegiance, magic tree
other allegiance
wizard attack occupied mtree
monster attack mtree
already moved
attack undead: no can do, ranged also, undead on undead, raised on
undead, magic weapon combos: sword ok, bow - ranged but not h-h, sword
and bow - both.
engaged and move
