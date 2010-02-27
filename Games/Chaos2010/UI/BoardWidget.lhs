
> module Games.Chaos2010.UI.BoardWidget where

> --import Control.Applicative
> import Database.HaskellDB
> --import Control.Monad as M
>
> import Games.Chaos2010.HaskellDBUtils
> import Games.Chaos2010.UI.UITypes
>
> import Games.Chaos2010.Database.Board_sprites1_view
> import Games.Chaos2010.Database.Fields

> boardWidget :: DBSpriteGrid
> boardWidget = DBSpriteGrid $ \db ->
>   do
>     t <- query db readPieces
>     return $ SpriteGrid 15 10 $ flip map t (\r -> (mn $ r # x, mn $ r # y, mv $ r # sprite))
>   where
>     readPieces = do
>       t1 <- table board_sprites1_view
>       project $ copy x t1
>                  .*. copy y t1
>                  .*. copy sprite t1
>                  .*. emptyRecord
