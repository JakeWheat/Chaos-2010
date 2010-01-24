
> module Games.Chaos2010.UI.InfoWidget (infoWidgetNew) where

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

= Info Widget

Displays the following information:
turn phase
spell info
cursor info
piece info for pieces on current square
piece info for selected piece

> infoWidgetNew :: Connection -> ColourList -> SpriteMap -> IO (TextView, IO ())
> infoWidgetNew conn colours spriteMap = do
>   tv <- myTextViewNew colours
>   fk <- forkAndQueueOneNew
>   let refresh = lg "infoWidgetNew.refresh" "" $ do
>         forkItemReplace fk conn tv "infoWidgetNew.refresh" $
>                    concat [turnPhaseInfo
>                           ,spellInfo
>                           ,cursorInfo
>                           ,cursorPieces
>                           ,selectedPieceInfo
>                           ]
>   return (tv, refresh)
>     where

== Turn phase info:
shows turn number, world alignment, turn phase, wizard up, continue button
if move phase, say how many pieces are left to move
TODO: add some text to tell the player what options he has or what he is
supposed to be doing

>       turnPhaseInfo = [

>         D.SelectValueIf "select * from turn_number_table" [] $
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
>        ]

== spell info:
only displayed if the current wizard has selected a spell
 (either in the choose phase or cast phase)
list info about the spell (where applicable):
name
spell sprite
type
implementation type
base chance
current chance
alignment
description
range
max pieces summoned
ptype of pieces summoned - same as sprite
number of shots with spell

in casting phase, if spell has more than one part (server or client side)
then this widget show how many parts remain to be cast

>       spellInfo = [

>         D.SelectTupleIf
>           "select * from current_wizard_selected_spell_details" [] $
>           \sd -> [
>               Text $ "\nChosen spell: " ++
>                    lk "spell_name" sd ++ "\t"
>              ,sprite $ lk "sprite" sd

>              ,Text $ "\n(" ++
>                             lk "spell_category" sd ++ ", " ++
>                             lk "alignment_string" sd ++ ", copies " ++
>                             lk "count" sd ++ ")" ++
>                             "\n" ++ lk "description" sd ++
>                             "\nchance " ++ lk "chance" sd ++ "% " ++
>                             " (base " ++ lk "base_chance" sd ++ "%)"

draw the extra spell casting info:
i think this code has got a bit stale and needs looking at again to
check the fields and field names

>              ,let fields = flip filter [("#pieces", "num")
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
>        ]

== cursor info
shows the cursor position
and piece info (see below) for each piece
on the square the cursor is on

>       cursorInfo = [
>         D.SelectTupleIf "select x,y from cursor_position" [] $
>                \cp -> [Text $ "\n\
>                               \---------\n\
>                               \\ncursor: " ++ lk "x" cp ++ ", " ++ lk "y" cp]
>        ]


== draw piece info helper

factor out this code since we want to draw piece info
for the selected piece and any pieces (could be up to three)
on the square the cursor is on

>       pieceInfo pit = [
>         Text "\n"
>        ,sprite $ lk "sprite" pit] ++
>        (case True of
>           _ | lk "ptype" pit == "wizard" ->
>                 [TaggedText (lk "allegiance" pit) [lk "colour" pit]]
>             | lk "dead" pit == "true" ->
>                 [TaggedText ("dead " ++ lk "ptype" pit ++ "-" ++ lk "tag" pit)
>                             ["grey"]]
>             | otherwise -> [
>                   Text (lk "ptype" pit ++ "-" ++ lk "tag" pit ++ "(")
>                  ,TaggedText (lk "allegiance" pit)
>                              [lk "colour" pit]
>                  ,Text ")"]) ++

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

>        [let booleanStats = ["flying"
>                            ,"undead"
>                            ,"ridable"
>                            ,"imaginary"
>                            ,"shadow_form"
>                            ,"magic_sword"
>                            ,"magic_knife"
>                            ,"magic_shield"
>                            ,"magic_wings"
>                            ,"magic_armoxur"
>                            ,"magic_bow"
>                            ,"computer_controlled"]
>             pieceBoolStats = filter (\s -> lk s pit == "true") booleanStats
>         in Text $ '\n' : intercalate ", " pieceBoolStats
>        ] ++

>        let atts = ["remaining_walk"
>                   ,"move_phase"
>                   ,"attack_strength"
>                   ,"physical_defense"
>                   ,"speed"
>                   ,"agility"
>                   ,"ranged_weapon_type"
>                   ,"range"
>                   ,"ranged_attack_strength"
>                   ,"magic_defense"
>                   ,"place"]
>            vals = zip atts $ map (\f -> lk f pit) atts
>            vals' = filter (\(_,f2) -> f2 /= "") vals
>        in map (\(f1,f2) -> Text $ "\n" ++ f1 ++ ": " ++ f2) vals'


== selected piece info
only displayed if there is a selected piece (in move phase)
show piece info for selected piece
which subphase the piece is in
squares left to move if walker and in moving subphase

>       selectedPieceInfo =
>         [D.SelectTupleIf "select * from selected_piece_details" [] $
>            \pd -> (Text "\n\n\
>                        \--------\n\
>                        \\nSelected piece:" : pieceInfo pd)]

== piece info subsubwidget
shows stats for the piece from:
pieces and sub entity tables
wizard upgrades

>       cursorPieces =
>         [D.SelectTuples "select * from cursor_piece_details order by sp" [] $
>            \cpd -> (Text "\n\
>                        \-------\n" : pieceInfo cpd)]

>       sprite s = let (pb,_,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)
