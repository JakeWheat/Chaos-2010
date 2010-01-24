
> module Games.Chaos2010.UI.SpellBookWidget (spellBookWidgetNew) where

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad

> import Games.Chaos2010.Dbms.ChaosDB
> import Games.Chaos2010.Utils
> import Games.Chaos2010.UI.MyTextView as MyTextView
> import qualified Games.Chaos2010.UI.DBTextView as D
> import qualified Games.Chaos2010.Misc.Logging as Logging
> import Games.Chaos2010.ChaosTypes
> import Games.Chaos2010.Misc.ThreadingUtils
> import Games.Chaos2010.UI.UIThreadingUtils
> import Games.Chaos2010.UI.ChaosTextView

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

> spellBookWidgetNew :: Connection ->
>                       ColourList ->
>                       SpriteMap ->
>                       IO (TextView, IO())
> spellBookWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   fk <- forkAndQueueOneNew
>   let refresh = lg "spellBookWidgetNew.refresh" "" $
>         forkItemReplace fk conn tv "spellBookWidgetNew.refresh" items
>   return (tv, refresh)
>     where


show help text

>       items = [D.SelectValueIf "select * from spell_book_show_all_table" [] $
>                     \s -> [Text $ (if s == "True"
>                                    then "hide unavailable spells - DELETE"
>                                    else "show all spells - INSERT") ++
>                                           "\n0 - unselect spell"]
>               ,D.SelectTuples "select * from spell_book_table\n\
>                               \order by section_order, alignment_order,\n\
>                               \base_chance desc, spell_name" [] $

write this spell's line

>                  \sb -> [Text "\n"
>                         ,TaggedText (lk "key" sb ++ " - ") [lk "colour" sb]
>                         ,sprite $ lk "sprite" sb
>                         ,TaggedText (" " ++ lk "spell_name" sb ++
>                                       " " ++ lk "chance" sb ++ "%")
>                                     [lk "colour" sb]
>                         ,Text $ " " ++ lk "align_icons" sb ++ " " ++
>                            lk "count_icons" sb
>                         ]
>               ]

see if we need to write a new category header - todo: use this somehow

 >       writeLine sc sb = [Text $ "\n\
 >                                 \-------------\n\
 >                                 \\n" ++ sb "spell_category" ++
 >                                   " spells:" | sb "spell_category" /= sc]

>       sprite s = let (_,pb,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)
