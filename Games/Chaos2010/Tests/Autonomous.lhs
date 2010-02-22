
> module Games.Chaos2010.Tests.Autonomous (autonomous) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.List
> import Control.Monad
> import Data.Maybe
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Exception

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HaskellDB
> import Database.HDBC

> import Games.Chaos2010.Tests.BoardUtils
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Database.Spell_books
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

> testCastleDisappear :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastleDisappear db = tctor "testCastleDisappear" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
>   startNewGameReadyToAuto db conn ("\n\
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

> testCastleStay :: IConnection conn => Database -> conn -> Test.Framework.Test
> testCastleStay db = tctor "testCastleStay" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('O', [makePD "wizard" "Buddha",
>                    makePD "dark_citadel" "Buddha"]),
>             ('C', [makePD "dark_citadel" "Buddha"])]
>   startNewGameReadyToAuto db conn ("\n\
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


> testGetSpell :: IConnection conn => Database -> conn -> Test.Framework.Test
> testGetSpell db = tctor "testGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
>   numSpells <- countSpells db
>   startNewGameReadyToAuto db conn ("\n\
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
>   newNumSpells <- countSpells db
>   assertEqual "got a new spell" (numSpells + 1) newNumSpells

> countSpells :: Database -> IO Int
> countSpells db = do
>   rel <- query db $ do
>                 t1 <- table spell_books
>                 restrict ((t1 .!. wizard_name) .==. constant "Buddha")
>                 project $ xid .=. count (t1 .!. xid)
>                         .*. emptyRecord
>   let t = head rel
>   return $ t # xid

> testNoGetSpell :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNoGetSpell db = tctor "testNoGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
>   startNewGameReadyToAuto db conn ("\n\
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

