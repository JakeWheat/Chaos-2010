
> module Games.Chaos2010.Tests.AllTests (allTests) where
>
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Exception

> import Database.HaskellDB.HDBC.PostgreSQL
> import Database.HaskellDB
> import Database.HDBC.PostgreSQL
> import Database.HDBC


> import Games.Chaos2010.Tests.Basics
> import Games.Chaos2010.Tests.Phases
> import Games.Chaos2010.Tests.Casting
> import Games.Chaos2010.Tests.Upgrades
> import Games.Chaos2010.Tests.Subphases
> import Games.Chaos2010.Tests.MoveMisc
> import Games.Chaos2010.Tests.Autonomous
> import Games.Chaos2010.Tests.Complete

>
> allTests :: Database -> Connection -> [Test]
> allTests db conn = map (\x -> x db conn)
>                    [basics
>                    ,phases
>                    ,casting
>                    ,upgrades
>                    ,subphases
>                    ,moveMisc
>                    ,autonomous
>                    ,complete]
