
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


> module Games.Chaos2010.Tests.SquaresValid (squaresValid) where

> import Test.HUnit
> import Test.Framework
> import Control.Monad

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.DBUpdates

> squaresValid :: IConnection conn => Database -> conn -> Test.Framework.Test
> squaresValid db conn = testGroup "squaresValid" [
>                   testSquareCategories db conn
>                  ]

> testSquareCategories :: IConnection conn => Database -> conn -> Test.Framework.Test
> testSquareCategories db = tctor "testSquareCategories" $ \conn -> do
>   startNewGame conn



stage 2:
test the valid target actions, the target actions are:
cast_target_spell (split by category)
select_piece_at_position
walk
fly
attack (walk, fly)
ranged_attack
