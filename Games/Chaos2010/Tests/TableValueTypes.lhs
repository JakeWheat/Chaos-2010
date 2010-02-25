> {-# LANGUAGE FlexibleContexts,TemplateHaskell #-}
> module Games.Chaos2010.Tests.TableValueTypes where

> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.Database.Board_size
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_number_table
> import qualified Games.Chaos2010.Database.Current_wizard_table as Cw
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Database.Pieces
> import qualified Games.Chaos2010.Database.Imaginary_pieces as I
> import qualified Games.Chaos2010.Database.Crimes_against_nature as Cr
> import qualified Games.Chaos2010.Database.Spell_books as Sb
> import qualified Games.Chaos2010.Database.Cursor_position as C
> import qualified Games.Chaos2010.Database.Wizard_display_info as Wd
> import Games.Chaos2010.Database.Game_completed_table
> --import Games.Chaos2010.Database.Cast_alignment_table
> --import Games.Chaos2010.Database.Remaining_walk_table
> --import qualified Games.Chaos2010.Database.Selected_piece as Sp
> --import qualified Games.Chaos2010.Database.Pieces_moved as Pm
> --import Games.Chaos2010.Database.Spell_parts_to_cast_table
> import qualified Games.Chaos2010.Database.Wizard_spell_choices_mr as Wc
> --import qualified Games.Chaos2010.Database.Action_history_mr as Ah
> --import Games.Chaos2010.Database.Cast_success_checked_table
> --import Games.Chaos2010.Database.Test_action_overrides

> $(makeValueTypes [[t|Board_size|]
>                  ,[t|World_alignment_table|]
>                  ,[t|Turn_number_table|]
>                  ,[t|Cw.Current_wizard_table|]
>                  ,[t|Turn_phase_table|]
>                  ,[t|Wizards|]
>                  ,[t|Pieces|]
>                  ,[t|Sb.Spell_books|]
>                  ,[t|I.Imaginary_pieces|]
>                  ,[t|Cr.Crimes_against_nature|]
>                  ,[t|Game_completed_table|]
>                  ,[t|Wc.Wizard_spell_choices_mr|]
>                  ,[t|C.Cursor_position|]
>                  ,[t|Wd.Wizard_display_info|]
>                  ])

