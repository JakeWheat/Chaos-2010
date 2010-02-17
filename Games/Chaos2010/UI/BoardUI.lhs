
> module Games.Chaos2010.UI.BoardUI where

> import Control.Applicative
> import Database.HaskellDB
> import Control.Monad as M
>
> import Games.Chaos2010.UI.HdbUtils
> import Games.Chaos2010.UI.UITypes
>
> import Games.Chaos2010.Database.Board_sprites1_view

> board :: SpriteGridReader
> board = SpriteGridReader $ \db ->
>   do
>     t <- query db readPieces
>     return $ flip map t (\r -> (mn $ r # x, mn $ r # y, mv $ r # sprite))
>   where
>     readPieces = do
>       t1 <- table board_sprites1_view
>       project $ copy x t1
>                  .*. copy y t1
>                  .*. copy sprite t1
>                  .*. emptyRecord

> data SpriteGridReader = SpriteGridReader (Database -> IO SpriteGrid)

