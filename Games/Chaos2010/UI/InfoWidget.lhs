Copyright 2010 Jake Wheat

> module Games.Chaos2010.UI.InfoWidget (infoWidget) where

> import Games.Chaos2010.UI.UITypes
> import Control.Applicative
> import Database.HaskellDB

> import Games.Chaos2010.Database.Turn_number_table

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
> turnPhaseInfo db = do
>   res <- query db $ table turn_number_table
>   return $ flip map res $ \r ->
>       Text $ "Turn " ++ show (r # turn_number)


> {-        D.SelectValueIf "select * from turn_number_table" [] $
>           \tn -> [Text $ "Turn " ++ tn ++ ", "]

>        ,D.SelectValueIf "select format_alignment(world_alignment)\n\
>                         \    as alignment\n\
>                         \  from world_alignment_table" [] $
>           \wa -> [Text $ "world alignment " ++ wa ++ ", "]

>        ,D.SelectValueIf "select turn_phase from turn_phase_table" [] $
>           \tp -> [Text $ "turn_phase " ++ tp ++ "\n"]
>        ,D.SelectTuples "select help from prompt" [] $
>           \tp -> [Text $ lk "help" tp ++ "\n"]
>        --,D.Items [MyTextView.Button "continue" $ dbAction conn "next_phase" []]
>        ,D.SelectTupleIf "select current_wizard,colour,allegiance,sprite \n\
>                         \  from current_wizard_table\n\
>                         \  inner join allegiance_colours\n\
>                         \  on current_wizard = allegiance\n\
>                         \  natural inner join wizard_sprites;" [] $
>             \wi -> [
>                     Text "\nWizard up: "
>                    ,TaggedText (lk "current_wizard" wi)
>                                [lk "colour" wi]
>                    ,sprite $ lk "sprite" wi
>                    ]

>        ,D.SelectValueIf "select count from\n\
>                         \    (select count(*) from pieces_to_move) as a\n\
>                         \cross join turn_phase_table\n\
>                         \where turn_phase='move';" [] $
>                         \ptm -> [Text $ "\nPieces left to move: " ++ ptm]
>        ] -}





> spellInfo :: Database -> IO [MyTextItem]
> spellInfo db = return [Text "spell info\n"]

> cursorInfo :: Database -> IO [MyTextItem]
> cursorInfo db = return [Text "cursor info\n"]

> cursorPieces :: Database -> IO [MyTextItem]
> cursorPieces db = return [Text "cursor pieces\n"]

> selectedPieceInfo :: Database -> IO [MyTextItem]
> selectedPieceInfo db = return [Text "selected piece info\n"]
