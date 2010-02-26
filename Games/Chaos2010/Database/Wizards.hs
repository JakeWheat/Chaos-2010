{-# LANGUAGE EmptyDataDecls, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fcontext-stack51  #-}
module Games.Chaos2010.Database.Wizards where
import Games.Chaos2010.Database.Fields
import Database.HaskellDB.DBLayout
 
type Wizards =
     Record
       (HCons (LVPair Wizard_name (Expr String))
          (HCons (LVPair Shadow_form (Expr Bool))
             (HCons (LVPair Magic_sword (Expr Bool))
                (HCons (LVPair Magic_knife (Expr Bool))
                   (HCons (LVPair Magic_shield (Expr Bool))
                      (HCons (LVPair Magic_wings (Expr Bool))
                         (HCons (LVPair Magic_armour (Expr Bool))
                            (HCons (LVPair Magic_bow (Expr Bool))
                               (HCons (LVPair Computer_controlled (Expr Bool))
                                  (HCons (LVPair Original_place (Expr Int))
                                     (HCons (LVPair Expired (Expr Bool)) HNil)))))))))))
 
wizards :: Table Wizards
wizards = baseTable "wizards"