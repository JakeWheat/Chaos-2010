Copyright 2010 Jake Wheat

> module Games.Chaos2010.UI.InfoWidget (infoWidget) where

> import Games.Chaos2010.UI.UITypes
> import Control.Applicative
> import Database.HaskellDB
> import Control.Monad as M

> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Prompt
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Allegiance_colours as Ac
> import Games.Chaos2010.Database.Wizard_sprites
>
>
> infoWidget :: DBText
> infoWidget =
>   DBText (\db -> concat <$> mapM (\r -> r db) bits)
>   where
>     bits = [turnPhaseInfo
>            ,spellInfo
>            ,cursorInfo
>            ,cursorPieces
>            ,selectedPieceInfo]
>
> turnPhaseInfo :: Database -> IO [MyTextItem]
> turnPhaseInfo db =
>   concat . concat <$> M.sequence [
>         q (table turn_number_table)
>           (\r -> [Text $ "Turn " ++ show (r # turn_number)])
>        ,q (table world_alignment_table) --todo: should be format_alignment(world_alignment)
>           (\r -> [Text $ ", world alignment " ++ show (r # world_alignment)])
>        ,q (table turn_phase_table)
>           (\r -> [Text $ ", turn phase  " ++ show (r # turn_phase)])
>        ,q (table prompt)
>           (\r -> [Text $ "\n" ++ mv (r # help)])
>        ,q (do
>             cwt <- table current_wizard_table
>             ac <- table allegiance_colours
>             ws <- table wizard_sprites
>             restrict ((cwt .!. current_wizard) .==.
>                       jn (ac .!. allegiance))
>             restrict ((cwt .!. current_wizard) .==.
>                       jn (ws .!. wizard_name))
>             project $ copy current_wizard cwt
>                       .*. copy Ac.colour ac
>                       .*. copy allegiance ac
>                       .*. copy sprite ws
>                       .*. emptyRecord)
>           (\r -> [Text $ "\nWizard up: "
>                  ,TaggedText [mv $ r # Ac.colour] (r # current_wizard)
>                  ,Text $ mv (r # sprite)]) -- todo: should be a sprite
>        {-,q (do
>        ,D.SelectValueIf "select count from\n\
>                         \    (select count(*) from pieces_to_move) as a\n\
>                         \cross join turn_phase_table\n\
>                         \where turn_phase='move';" [] $
>                         \ptm -> [Text $ "\nPieces left to move: " ++ ptm]
>           (\r -> [])-}
>       ]
>   where
>     jn = fromNull (constant "")
>     mv = maybe "" id
>     q t r = query db t >>=
>             return . map r


> spellInfo :: Database -> IO [MyTextItem]
> spellInfo db = return [Text "spell info\n"]

> cursorInfo :: Database -> IO [MyTextItem]
> cursorInfo db = return [Text "cursor info\n"]

> cursorPieces :: Database -> IO [MyTextItem]
> cursorPieces db = return [Text "cursor pieces\n"]

> selectedPieceInfo :: Database -> IO [MyTextItem]
> selectedPieceInfo db = return [Text "selected piece info\n"]
