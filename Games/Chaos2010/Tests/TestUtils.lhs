
> {-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, FlexibleContexts #-}
> module Games.Chaos2010.Tests.TestUtils
>     (tctor
>
>     ,goSquare
>
>     ,queryTurnPhase
>     ,queryTurnSequenceInfo
>     ,querySelectedPiece
>
>     ,assertCurrentWizardPhase
>     ,assertSelectedPiece
>     ,assertMoveSubphase
>     ,assertPieceDoneSelection
>     ,assertNoSelectedPiece
>
>     ) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Control.Monad
> --import Control.Exception
> --import Data.List

> import Database.HDBC (IConnection, withTransaction)
> import Database.HaskellDB

> import Games.Chaos2010.DBUpdates hiding (rigActionSuccess)
> --import qualified Games.Chaos2010.DBUpdates as DBu
> import Games.Chaos2010.Database.Selected_piece
> import Games.Chaos2010.Database.Pieces_moved
> import Games.Chaos2010.Database.Turn_phase_table
> import Games.Chaos2010.Database.Current_wizard_table
> import Games.Chaos2010.Database.Turn_number_table
> import Games.Chaos2010.Database.Fields
> import qualified Games.Chaos2010.Utils as U

====================================================================

> tctor :: IConnection conn => String
>       -> (conn -> IO ())
>       -> conn
>       -> Test.Framework.Test
> tctor l f conn =
>   testCase l $ withTransaction conn $ \c -> U.time $ f c

> goSquare :: IConnection conn => Database -> conn -> Int -> Int -> IO ()
> goSquare db conn xp yp = do
>   setCursorPos db conn xp yp
>   actionGo db conn --sendKeyPress conn "Return"


================================================================================

= database read shortcuts

> queryTurnPhase :: Database -> IO String
> queryTurnPhase db = do
>   rel <- query db $ table turn_phase_table
>   let t = head rel
>   return (t # turn_phase)

> {-queryCurrentWizard :: Database -> IO String
> queryCurrentWizard db = do
>   rel <- query db $ table current_wizard_table
>   let t = head rel
>   return (t # current_wizard)-}

> queryTurnSequenceInfo :: Database
>                       -> IO
>                          [Record
>                           (HCons
>                            (LVPair Turn_number Int)
>                            (HCons
>                             (LVPair Current_wizard String)
>                             (HCons
>                              (LVPair Turn_phase String)
>                              HNil)))]
> queryTurnSequenceInfo db = do
>   r1 <- query db $ do
>           t1 <- table turn_number_table
>           t2 <- table current_wizard_table
>           t3 <- table turn_phase_table
>           project $ copyAll t1 `hAppend` copyAll t2 `hAppend` copyAll t3
>   return r1

> type Selected_piece_v =
>     Record (HCons (LVPair Ptype String)
>             (HCons (LVPair Allegiance String)
>              (HCons (LVPair Tag Int)
>               (HCons (LVPair Move_phase String)
>                (HCons (LVPair Engaged Bool) HNil)))))
> querySelectedPiece :: Database -> IO [Selected_piece_v]
> querySelectedPiece db =
>   query db $ table selected_piece


1 turn number
1 current wizard
1 turn phase
m wizard spellchoices
01 spell parts to cast
01 cast success checked
1 world alignment
m pieces moved
01 selected piece
01 remaining walk
01 cast alignment

> assertSelectedPiece :: Database -> String -> String -> IO()
> assertSelectedPiece db pt al = do
>   rel <- query db $ table selected_piece
>   let t = head rel
>   assertEqual "selected piece" (pt,al)
>               (t # ptype, t # allegiance)

> assertCurrentWizardPhase :: Database -> String -> String -> IO()
> assertCurrentWizardPhase db wiz phase = do
>   r <- queryTurnSequenceInfo db
>   assertEqual "" (makeCwTp $ head r)
>                  (current_wizard .=. wiz
>                   .*. turn_phase .=. phase
>                   .*. emptyRecord)

> makeCwTp :: (HasField Turn_phase r v1, HasField Current_wizard r v) =>
>             r -> Record (HCons
>                          (LVPair Current_wizard v)
>                          (HCons (LVPair Turn_phase v1) HNil))
> makeCwTp hl =
>     current_wizard .=. (hl # current_wizard)
>      .*. turn_phase .=. (hl # turn_phase)
>      .*. emptyRecord

 > projectT (l:ls) hl =
 >     (l .=. (hl # l)) .*. projectT ls hl
 > projectT [] hl = undefined --emptyRecord


> assertPieceDoneSelection :: Database -> String -> String -> IO ()
> assertPieceDoneSelection db pt al = do
>     assertNoSelectedPiece db
>     rel <- query db $ do
>              t1 <- table pieces_moved
>              restrict ((t1 .!. ptype) .==. constant pt)
>              restrict ((t1 .!. allegiance) .==. constant al)
>              project $ copyAll t1
>     assertBool "piece in ptm" (not $ null rel)

> assertMoveSubphase :: Database -> [Char] -> IO ()
> assertMoveSubphase db sph = do
>   rel <- query db $ do
>            t1 <- table selected_piece
>            project $ copy move_phase t1
>                      .*. emptyRecord
>   let t = head rel
>   assertEqual "move subphase" sph (t # move_phase)

> assertNoSelectedPiece :: Database -> IO ()
> assertNoSelectedPiece db = do
>   rel <- query db $ table selected_piece
>   assertBool ("there should be no selected piece, got " ++
>               let r = head rel
>               in r # ptype ++ " - " ++ r # allegiance ++
>                  " " ++ r # move_phase)
>              (null rel)


================================================================================

= keyboard stuff

create wrappers around the key press stuff to make the tests easier to
understand

> {-lookupChooseSpellKeys :: [([Char], [Char])]
> lookupChooseSpellKeys = [("goblin", "m")
>                         ,("disbelieve", "Q")
>                         ,("magic_wood", "G")
>                         ,("shadow_wood", "M")
>                         ,("magic_bolt", "D")
>                         ,("vengeance", "E")
>                         ,("subversion", "R")
>                         ,("raise_dead", "V")
>                         ,("magic_knife", "1")
>                         ,("magic_shield", "2")
>                         ,("magic_armour", "3")
>                         ,("magic_bow", "4")
>                         ,("magic_sword", "5")
>                         ,("shadow_form", "6")
>                         ,("magic_wings", "7")
>                         ,("law", "O")
>                         ]


> keyChooseSpell :: String -> String
> keyChooseSpell spellName = U.safeLookup
>                              "get key for spell" spellName
>                              lookupChooseSpellKeys-}
