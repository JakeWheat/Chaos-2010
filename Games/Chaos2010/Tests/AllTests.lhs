
> module Games.Chaos2010.Tests.AllTests (allTests) where
>
> import Test.Framework

> import Database.HaskellDB
> import Database.HDBC (IConnection)

> import Games.Chaos2010.Tests.Basics
> import Games.Chaos2010.Tests.Phases
> import Games.Chaos2010.Tests.Casting
> import Games.Chaos2010.Tests.Upgrades
> import Games.Chaos2010.Tests.Subphases
> import Games.Chaos2010.Tests.MoveMisc
> import Games.Chaos2010.Tests.Autonomous
> import Games.Chaos2010.Tests.Complete
> import Games.Chaos2010.Tests.SquaresValid
>
> allTests :: IConnection conn => Database -> conn -> [Test]
> allTests db conn = map (\x -> x db conn)
>                    [basics
>                    ,phases
>                    ,casting
>                    ,upgrades
>                    ,subphases
>                    ,moveMisc
>                    ,autonomous
>                    ,complete
>                    ,squaresValid
>                    ]


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

