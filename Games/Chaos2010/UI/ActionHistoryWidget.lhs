> module Games.Chaos2010.UI.ActionHistoryWidget where

> import Control.Applicative
> import Database.HaskellDB
>
> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.UI.HdbUtils
> import Games.Chaos2010.Database.Action_history_colour_mr


> actionHistoryWidget :: DBText
> actionHistoryWidget =
>   DBText $ \db ->
>   let q t r = qdb db t r
>   in concat <$>

"select * from action_history_colour_mr\n\
                           \where id > ?\n\
                           \order by id"

>         q (do
>            t <- table action_history_colour_mr
>            order [asc t xid]
>            project $ copyAll t)
>           (\r ->
>            let thistory_name = mv (r # history_name)
>                cTag = [mv $ r # colour]
>                tallegiance = mv $ r # allegiance
>                wc = TaggedText cTag tallegiance
>                pt = [Text $ mv (r # ptype) ++ "-"
>                     ,TaggedText cTag $ tallegiance
>                     ,Text $ '-' : smn (r # tag)]
>                tnum_wizards = smn $ r # num_wizards
>                tspell_name = mv $ r # spell_name
>                tturn_phase = mv $ r # turn_phase
>                tturn_number = smn $ r # turn_number
>            in (Text $ smn (r # xid) ++ ". ") :
>               (case thistory_name of
>                  "new_game" ->
>                      [Text $ "new game: " ++ tnum_wizards ++
>                         " wizards are well up for a ruck"]
>                  "choose_spell" ->
>                      [wc
>                      ,TaggedText ["yellow"] (" chose " ++ tspell_name)]
>                  "wizard_up" ->
>                      [Text "wizard_up: "
>                      ,TaggedText cTag tallegiance
>                      ,Text $ " - " ++ tturn_phase ++ " phase"]
>                  "spell_skipped" ->
>                      [wc
>                      ,TaggedText ["yellow"] $ " skipped casting " ++ tspell_name]
>                  "spell_succeeded" ->
>                      [wc
>                      ,TaggedText ["green"] $ " successfully cast " ++ tspell_name]
>                  "spell_failed" ->
>                      [wc
>                      ,TaggedText ["red"] $ " failed to cast " ++ tspell_name]
>                  "chinned" ->
>                      pt ++ [Text " was chinned"]
>                  "shrugged_off" ->
>                      pt ++ [Text " shrugged off the attack"]
>                  "moved" ->
>                      pt ++ [Text " moved"]
>                  "attack" ->
>                      pt ++ [Text " attacked"]
>                  "new_turn" ->
>                      [Text $ "New turn: " ++ tturn_number]
>                  "game_won" ->
>                      [wc
>                      ,Text " has won!"]
>                  "game_drawn" ->
>                      [Text "the game is a draw."]
>                  _ -> [Text $ thistory_name ++ " FIXME"]) ++ [Text "\n"])


                       "set_imaginary"
                       "set_real"
                       "spread"
                       "recede"
                       "disappear"
