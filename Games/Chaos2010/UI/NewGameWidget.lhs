
= New Game Widget

Starting new game involves the following choices:
number of wizards (2-8)
computer wizards same ai same stats as player
for each wizard:
    name text - autogenerated, can be changed
    computer_controlled bool
    sprite and colour displayed but cannot currently be changed

new game widget:
-------------------------------------------------------------
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
|sprite | autogen name (editable) |radio:human computer none|
-------------------------------------------------------------


> module Games.Chaos2010.UI.NewGameWidget where
>
> import Control.Applicative
> import Database.HaskellDB
> import Control.Monad as M

> import Games.Chaos2010.UI.UITypes
> import Games.Chaos2010.Database.New_game_widget_state
> import Games.Chaos2010.UI.HdbUtils
>
> newGameWidget :: DBText
> newGameWidget =
>    DBText $ \db ->
>    let q t r = qdb db t r
>    in concat . concat <$> M.sequence [
>       q (do
>           t1 <- table new_game_widget_state
>           order [asc t1 line]
>           project $ copyAll t1)
>         (\r -> [Image $ "medium-" ++ r # sprite
>                ,Text $ "\t" ++ r # wizard_name ++ "\t"
>                ,ToggleButtonGroup [("human", "human")
>                                   ,("computer","computer")
>                                   ,("none", "none")]
>                                   (show (r # line))
>                                   (r # state)
>                ,Text "\n"])
>      ,q (do
>          t1 <- table new_game_widget_state
>          project $ line .=. count(t1 .!. line)
>                 .*. emptyRecord)
>         (\_ -> [Button "start game" "startGame"
>                ,Button "reset this window" "resetNewGameWindow"
>                ,Text "\n"]
>          ++ flip map ["all_pieces"
>                      ,"upgraded_wizards"
>                      ,"overlapping"]
>                      (\lb -> Button lb lb))]


add the buttons at the bottom of the panel to start the game and
reset the panel

> {-              ,D.Items $ [MyTextView.Button "start game" $
>                             forkIt $ dbAction conn
>                                      "client_new_game_using_\
>                                      \new_game_widget_state" []
>                             --temp:
>                             --win <- getParentWidget tv
>                             --widgetHideAll win
>                          ,MyTextView.Button "reset this window" $ forkIt $ do
>                             dbAction conn "reset_new_game_widget_state" []
>                             refresh
>                          ,Text "\n"
>                          ] ++

add some temporary buttons to start custom games for testing purposes

>                           for ["all_pieces"
>                               ,"upgraded_wizards"
>                               ,"overlapping"
>                               ] (\l -> MyTextView.Button l $ forkIt $ do
>                                   dbAction conn
>                                            "client_new_game_using_\
>                                            \new_game_widget_state" []
>                                   dbAction conn "setup_test_board" [l])
>                                   -- need to call refresh all here somehow
>               ]
>       sprite s = let (pb,_,_) = safeMLookup ("show sprite " ++ s) s spriteMap
>                  in Pixbuf $ head pb-}

TODO: make this into a game manager which handles managing multiple
games and deleting ones you don't want, as well as starting new games
