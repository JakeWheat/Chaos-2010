Copyright 2010 Jake Wheat

> module Games.Chaos2010.UI.InfoWidget (infoWidget) where
>
> import Games.Chaos2010.UI.UITypes
> import Control.Applicative
> import Database.HaskellDB hiding (count)
> import Control.Monad as M
>
> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Prompt
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Allegiance_colours
> import Games.Chaos2010.Database.Wizard_sprites
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.Database.Cursor_piece_details
> import Games.Chaos2010.Database.Selected_piece_details
>
> import Games.Chaos2010.Database.Current_wizard_selected_spell_details
> import Games.Chaos2010.Database.Fields
>
> import Games.Chaos2010.HaskellDBUtils
>
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
>                       .*. copy colour ac
>                       .*. copy allegiance ac
>                       .*. copy sprite ws
>                       .*. emptyRecord)
>           (\r -> [Text $ "\nWizard up: "
>                  ,TaggedText [mv $ r # colour] (r # current_wizard)
>                  ,Image $ "medium-" ++ mv (r # sprite)]) -- todo: should be a sprite
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
>
> spellInfo :: Database -> IO [MyTextItem]
> spellInfo db =
>   concat . concat <$> M.sequence [
>         q (table current_wizard_selected_spell_details)
>           (\r -> [Text $ "\nChosen spell: " ++ mv (r # spell_name)
>                   ++ "\t"
>                  ,Image $ "medium-" ++ mv (r # sprite) -- should be sprite
>                  ,Text $ "\n(" ++ mv (r # spell_category) ++ ", "
>                        ++ mv (r # alignment_string) ++ ", copies "
>                        ++ show (r # count) ++ ")\n"
>                        ++ mv (r # description)
>                        ++ "\nchance " ++ show (r # chance) ++ "% "
>                        ++ " (base " ++ show (r # base_chance) ++"%)"
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
> cursorInfo db =
>   concat <$> q (do
>          t1 <- table cursor_position
>          project $ copy x t1
>                  .*. copy y t1
>                  .*. emptyRecord)
>          (\r -> [Text $ "\n\
>                         \---------\n\
>                         \\ncursor: " ++ show (r # x) ++ ", " ++ show (r # y)])
>   where
>     q t r = qdb db t r

>
> cursorPieces :: Database -> IO [MyTextItem]
> cursorPieces db =
>   concat <$> q (do
>          t1 <- table cursor_piece_details
>          order [asc t1 sp]
>          project $ copyAll t1)
>          (\r -> (Text "\n\
>                       \-------\n" : pieceInfo r))
>   where
>     q t r = qdb db t r
>
> selectedPieceInfo :: Database -> IO [MyTextItem]
> selectedPieceInfo db =
>   concat <$> q (table selected_piece_details)
>          (\r -> (Text "\n\n\
>                        \--------\n\
>                        \\nSelected piece:" : pieceInfo r))
>   where
>     q t r = qdb db t r


> pieceInfo r = []
>   {-[Text "\n"
>   ,Image r # Cpd.sprite ++
>    (case () of
>             _ | r # ptype == "wizard" ->
>                 [TaggedText [r # colour] $ r # allegiance]
>               | r # dead ->
>                 [TaggedText ["grey"] $ "dead " ++ r # ptype ++ "-" ++ t # tag]
>               | otherwise -> [
>                   Text $ r # ptype  ++ "-" ++ r # tag ++ "("
>                  ,TaggedText [r # colour] $ r # allegiance
>                  ,Text ")"])]-} {- ++

boolean stats are treated differently:
Don't display bool stats if they are false,
for all the stats which are true, display a csv list of their names
when multiplayer/ computer controlled:
TODO: hide imaginary? or make it optional:
if you are playing against the computer or other players over the network,
you might want to be able to see which of your monsters are imaginary
if you are playing with multiple players at one computer, you definitely
don't want to be able to see which monsters are imaginary (as with the
original chaos), cos then everyone can cheat. You should never be able
to see imaginary of monsters that aren't yours.

>        [let booleanStats = [flying
>                            ,undead
>                            ,ridable
>                            ,imaginary
>                            ,shadow_form
>                            ,magic_sword
>                            ,magic_knife
>                            ,magic_shield
>                            ,magic_wings
>                            ,magic_armour
>                            ,magic_bow
>                            ,computer_controlled]
>             pieceBoolStats = filter (\s -> r # s) booleanStats
>         in Text $ '\n' : intercalate ", " pieceBoolStats
>        ] ++

>        let atts = [remaining_walk
>                   ,move_phase
>                   ,attack_strength
>                   ,physical_defense
>                   ,speed
>                   ,agility
>                   ,ranged_weapon_type
>                   ,range
>                   ,ranged_attack_strength
>                   ,magic_defense
>                   ,place]
>            vals = zip atts $ map (\f -> r # f) atts
>            vals' = filter (\(_,f2) -> f2 /= "") vals
>        in map (\(f1,f2) -> Text $ "\n" ++ f1 ++ ": " ++ f2) vals' -}
