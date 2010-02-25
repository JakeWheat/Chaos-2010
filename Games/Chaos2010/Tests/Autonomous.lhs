
> module Games.Chaos2010.Tests.Autonomous (autonomous) where

> import Test.HUnit
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.SetupGameState
> import Games.Chaos2010.Tests.TestUtils
> import Games.Chaos2010.Utils
> import Games.Chaos2010.Database.Spell_books
>
> autonomous :: IConnection conn => Database -> conn -> Test.Framework.Test
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
>   newSetupGame db conn (setPhase "cast"
>                         . setCurrentWizard "Zarathushthra") ("\n\
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
>   newSetupGame db conn (setPhase "cast"
>                         . setCurrentWizard "Zarathushthra") ("\n\
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
>   newSetupGame db conn (setPhase "cast"
>                         . setCurrentWizard "Zarathushthra") ("\n\
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
>   numSpells <- countSpells db
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
> countSpells db =
>   getCount db $ do
>                 t1 <- table spell_books
>                 restrict ((t1 .!. wizard_name) .==. constant "Buddha")

> testNoGetSpell :: IConnection conn => Database -> conn -> Test.Framework.Test
> testNoGetSpell db = tctor "testNoGetSpell" $ \conn -> do
>   let pl = wizardPiecesList ++
>            [('T', [makePD "wizard" "Buddha",
>                    makePD "magic_tree" "Buddha"])]
>   newSetupGame db conn (setPhase "cast"
>                         . setCurrentWizard "Zarathushthra") ("\n\
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

