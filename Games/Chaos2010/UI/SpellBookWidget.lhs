
================================================================================

= Spell book widget

Basic idea

The spell book is a vertical widget with one line for each spell.

The spells always appear in the same order and are arranged by spell
type (wizard, attacking, object, misc, monster)

You can toggle whether you see all spells in the game with the ones
you don't have available to cast greyed out, or just see the spells
you have available.

Each spell line is:
Spell key - spell sprite spell name casting chance alignment number
e.g.
G - [W] magic wood 80% + #
this means press G to select magic wood, you have 80% chance of
success, it's alignment is law-1 and you have one copy
o - [O] orc 100% *  ##
this means you press o to select orc, you have 100% chance of
success, it's alignment is chaos-1 and you have 2 copies

almost all the work for this is done in the sql code

each spell you have available is coloured according to the spell cast
chance - the colours are from the original chaos:

white 100
yellow 90
cyan 70-80
green 50-60
purple 30-40
red 10-20

> module Games.Chaos2010.UI.SpellBookWidget where
> import Control.Applicative
> import Database.HaskellDB
> import Control.Monad as M
>
> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.Database.Spell_book_show_all_table
> import Games.Chaos2010.Database.Spell_book_table
> import Games.Chaos2010.UI.HdbUtils
>
> spellBookWidget :: DBText
> spellBookWidget =
>   DBText $ \db ->
>   let q t r = qdb db t r
>   in concat . concat <$> M.sequence [
>         q (table spell_book_show_all_table)
>           (\r -> [Text $ (if r # spell_book_show_all
>                           then "hide unavailable spells - DELETE"
>                           else "show all spells - INSERT")
>                                ++ "\n0 - unselect spell"])
>        ,q (do
>            t1 <- table spell_book_table
>            order [asc t1 section_order
>                  ,asc t1 alignment_order
>                  ,desc t1 base_chance
>                  ,asc t1 spell_name]
>            project $ copyAll t1)
>           (\r -> [Text "\n"
>                  ,TaggedText [mv $ r # colour] $ mv (r # key) ++ " - "
>                  ,Image $ mv $ r # sprite
>                  ,TaggedText [mv $ r # colour]
>                              $ " " ++ mv (r # spell_name)
>                                ++ " " ++ smn (r # chance) ++ "%"
>                  ,Text $ " " ++ mv (r # align_icons) ++ " "
>                          ++ mv (r # count_icons)
>                  ])
>              ]
