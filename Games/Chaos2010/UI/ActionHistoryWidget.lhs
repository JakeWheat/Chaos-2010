
> module Games.Chaos2010.UI.ActionHistoryWidget (actionHistoryWidgetNew) where

> import Graphics.UI.Gtk hiding (disconnect)
>
> import Data.List
> import qualified Data.Char as DC
> import Control.Monad
> import Data.IORef

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

= Action history Widget

This shows a log of all the actions that have happened in a game, so
you can see what's going on.

> actionHistoryWidgetNew :: Connection ->
>                           ColourList ->
>                           SpriteMap ->
>                           IO (TextView, IO())
> actionHistoryWidgetNew conn colours _ = do
>   tv <- myTextViewNew colours
>   textViewAddScrollToBottom tv

we save the id of the last history item shown. This is so when refresh
is called, it only needs to append the new ones to the bottom of the
text box instead of redrawing them all

>   lastHistoryIDBox <- newIORef (-1::Integer)
>   fk <- forkAndQueueOneNew
>   let refresh = lg "actionHistoryWidgetNew.refresh" "" $ do
>         forkUpdate fk "actionHistoryWidgetNew.refresh"
>           (do
>            is <- items lastHistoryIDBox
>            D.run conn [is])
>           (\r -> do
>                  render tv r
>                  textViewScrollToBottom tv)
>
>   return (tv, refresh)
>     where
>       items lhIDBox = do
>             lhID <- readIORef lhIDBox
>             return $ D.SelectTuplesIO
>                           "select * from action_history_colour_mr\n\
>                           \where id > ?\n\
>                           \order by id" [show lhID] $
>               \h -> do
>                 writeIORef lhIDBox $ read $ lk "id" h
>                 return $ th h ++ [Text "\n"]
>       th h = let cTag = [lk "colour" h]
>                  wc = TaggedText (lk "allegiance" h) cTag
>                  ptype = [Text $ lk "ptype" h ++ "-"
>                          ,TaggedText (lk "allegiance" h) cTag
>                          ,Text $ '-' : lk "tag" h]
>              in (Text $ lk "id" h ++ ". ") :

-- unfinished...

>                case lk "history_name" h of
>                  "new_game" ->
>                      [Text $ "new game: " ++ lk "num_wizards" h ++
>                         " wizards are well up for a ruck"]
>                  "choose_spell" ->
>                      [wc
>                      ,TaggedText (" chose " ++ lk "spell_name" h) ["yellow"]]
>                  "wizard_up" ->
>                      [Text "wizard_up: "
>                      ,TaggedText (lk "allegiance" h) cTag
>                      ,Text (" - " ++ lk "turn_phase" h ++ " phase")]
>                  "spell_skipped" ->
>                      [wc
>                      ,TaggedText (" skipped casting " ++ lk "spell_name" h)
>                                  ["yellow"]]
>                  "spell_succeeded" ->
>                      [wc
>                      ,TaggedText (" successfully cast " ++ lk "spell_name" h)
>                                  ["green"]]
>                  "spell_failed" ->
>                      [wc
>                      ,TaggedText (" failed to cast " ++ lk "spell_name" h)
>                                  ["red"]]
>                  "chinned" ->
>                      ptype ++ [Text " was chinned"]
>                  "shrugged_off" ->
>                      ptype ++ [Text " shrugged off the attack"]
>                  "moved" ->
>                      ptype ++ [Text " moved"]
>                  "attack" ->
>                      ptype ++ [Text " attacked"]
>                  "new_turn" ->
>                      [Text $ "New turn: " ++ lk "turn_number" h]
>                  "game_won" ->
>                      [wc
>                      ,Text " has won!"]
>                  "game_drawn" ->
>                      [Text "the game is a draw."]
>                  _ -> [Text $ lk "history_name" h ++ " FIXME"]

                       "set_imaginary"
                       "set_real"
                       "spread"
                       "recede"
                       "disappear"

 >       writeText = mapM_ writeItem
 >       writeItem i = case i of
 >                     Text t -> putStr t
 >                     TaggedText t _ -> putStr t
 >                     _ -> return ()

> lg :: String -> String -> IO c -> IO c
> lg l = Logging.pLog ("chaos.chaos." ++ l)
