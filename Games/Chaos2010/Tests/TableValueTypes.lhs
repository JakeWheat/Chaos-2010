> {-# LANGUAGE FlexibleContexts,TemplateHaskell #-}
> module Games.Chaos2010.Tests.TableValueTypes where

> import Games.Chaos2010.ThHdb
> import Games.Chaos2010.Database.Board_size
> import Games.Chaos2010.Database.World_alignment_table
> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Wizards
> import Games.Chaos2010.Database.Pieces
> import Games.Chaos2010.Database.Imaginary_pieces
> import Games.Chaos2010.Database.Crimes_against_nature
> import Games.Chaos2010.Database.Spell_books
> import Games.Chaos2010.Database.Cursor_position
> import Games.Chaos2010.Database.Wizard_display_info
> import Games.Chaos2010.Database.Game_completed_table
> import Games.Chaos2010.Database.Wizard_spell_choices_mr

> $(makeValueTypes [[t|Board_size|]
>                  ,[t|World_alignment_table|]
>                  ,[t|Turn_number_table|]
>                  ,[t|Current_wizard_table|]
>                  ,[t|Turn_phase_table|]
>                  ,[t|Wizards|]
>                  ,[t|Pieces|]
>                  ,[t|Spell_books|]
>                  ,[t|Imaginary_pieces|]
>                  ,[t|Crimes_against_nature|]
>                  ,[t|Game_completed_table|]
>                  ,[t|Wizard_spell_choices_mr|]
>                  ,[t|Cursor_position|]
>                  ,[t|Wizard_display_info|]
>                  ])

