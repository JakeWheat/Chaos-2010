
> module Games.Chaos2010.Tests.Autonomous (autonomous) where

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
> autonomous db conn = testGroup "autonomous" $
>                   map (\x -> x db conn)
>                       [testCastleDisappear
>                       ,testCastleStay
>                       ,testGetSpell
>                       ,testNoGetSpell
>                       ]

================================================================================

= autonomous action tests

fire, blob spreading

test single square spread success, fail

test single square disappear

test spreading onto square with pieces

castles disappearing

> testCastleDisappear :: Database -> Connection -> Test.Framework.Test
> testCastleDisappear db = tctor "testCastleDisappear" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
>   startNewGameReadyToAuto conn ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "disappear" True
>   sendKeyPress conn "space"
>   assertBoardEquals db ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

> testCastleStay :: Database -> Connection -> Test.Framework.Test
> testCastleStay db = tctor "testCastleStay" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
>   startNewGameReadyToAuto conn ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "disappear" False
>   sendKeyPress conn "space"
>   assertBoardEquals db ("\n\
>                   \O      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)


> testGetSpell :: Database -> Connection -> Test.Framework.Test
> testGetSpell db = tctor "testGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
>   numSpells <- selectInt conn
>          "select count(*) from spell_books where wizard_name='Buddha'" []
>   startNewGameReadyToAuto conn ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "bonus" True
>   sendKeyPress conn "space"
>   assertBoardEquals db ("\n\
>                   \1      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)
>   newNumSpells <- selectInt conn
>          "select count(*) from spell_books where wizard_name='Buddha'" []
>   assertEqual "got a new spell" (numSpells + 1) newNumSpells

> testNoGetSpell :: Database -> Connection -> Test.Framework.Test
> testNoGetSpell db = tctor "testNoGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
>   startNewGameReadyToAuto conn ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8", pl)
>   rigActionSuccess conn "bonus" False
>   sendKeyPress conn "space"
>   assertBoardEquals db ("\n\
>                   \T      2      3\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \4             5\n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \               \n\
>                   \6      7      8",pl)

