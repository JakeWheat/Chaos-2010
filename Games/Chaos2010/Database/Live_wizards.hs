{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack52   #-}
module Games.Chaos2010.Database.Live_wizards where
import Games.Chaos2010.Database.Fields
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Live_wizards =
     Record
       (HCons (LVPair Wizard_name (Expr (Maybe String)))
          (HCons (LVPair Shadow_form (Expr (Maybe Bool)))
             (HCons (LVPair Magic_sword (Expr (Maybe Bool)))
                (HCons (LVPair Magic_knife (Expr (Maybe Bool)))
                   (HCons (LVPair Magic_shield (Expr (Maybe Bool)))
                      (HCons (LVPair Magic_wings (Expr (Maybe Bool)))
                         (HCons (LVPair Magic_armour (Expr (Maybe Bool)))
                            (HCons (LVPair Magic_bow (Expr (Maybe Bool)))
                               (HCons (LVPair Computer_controlled (Expr (Maybe Bool)))
                                  (HCons (LVPair Original_place (Expr (Maybe Int)))
                                     (HCons (LVPair Expired (Expr (Maybe Bool)))
                                        (HCons (LVPair Place (Expr (Maybe Int))) HNil))))))))))))
 
live_wizards :: Table Live_wizards
live_wizards = baseTable "live_wizards"