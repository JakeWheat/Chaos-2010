Copyright 2010 Jake Wheat

> module Games.Chaos2010.UI.InfoWidget (infoWidget) where

> import Games.Chaos2010.UI.UITypes
> import Control.Applicative
> import Database.HaskellDB
> import Database.HaskellDB.Database
> import Control.Monad as M

> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Prompt
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Allegiance_colours as Ac
> import Games.Chaos2010.Database.Wizard_sprites
> import qualified Games.Chaos2010.Database.Current_wizard_selected_spell_details as Cwssd
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
>     q t r = qdb db t r

> qdb :: (GetRec er vr) =>
>        Database -> Query (Rel (Record er)) -> (Record vr -> b) -> IO [b]
> qdb db t r = query db t >>= return . map r

> jn :: Expr (Maybe String) -> Expr String
> jn = fromNull (constant "")

> mv :: Maybe String -> String
> mv = maybe "" id



> spellInfo :: Database -> IO [MyTextItem]
> spellInfo db =
>   concat . concat <$> M.sequence [
>         q (table Cwssd.current_wizard_selected_spell_details)
>           (\r -> [Text $ "\nChosen spell: " ++ mv (r # Cwssd.spell_name)
>                   ++ "\t"
>                  ,Text $ mv (r # Cwssd.sprite) -- should be sprite
>                  ,Text $ "\n(" ++ mv (r # Cwssd.spell_category) ++ ", "
>                        ++ mv (r # Cwssd.alignment_string) ++ ", copies "
>                        ++ show (r # Cwssd.count) ++ ")\n"
>                        ++ mv (r # Cwssd.description)
>                        ++ "\nchance " ++ show (r # Cwssd.chance) ++ "% "
>                        ++ " (base " ++ show (r # Cwssd.base_chance) ++"%)"
>                  ])
>        ]
>   where
>     q t r = qdb db t r

draw the extra spell casting info:
i think this code has got a bit stale and needs looking at again to
check the fields and field names

>  {-            ,let fields = flip filter [("#pieces", "num")
>                                        ,("parts", "parts")
>                                        ,("range", "range")
>                                        ]
>                                 (\(_,f) -> not (lk f sd == "" ||
>                                                 read (lk f sd) < (2::Int)))
>               in Text $ '\n' : intercalate ", "
>                                  (for fields
>                                       (\(n,f) ->
>                                        n ++ ": " ++ lk f sd))
>              ]
>        ] -}


> cursorInfo :: Database -> IO [MyTextItem]
> cursorInfo db = return [Text "cursor info\n"]

> cursorPieces :: Database -> IO [MyTextItem]
> cursorPieces db = return [Text "cursor pieces\n"]

> selectedPieceInfo :: Database -> IO [MyTextItem]
> selectedPieceInfo db = return [Text "selected piece info\n"]
