module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)

import Poly
import ParserSimple

{- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... -}


{- 

Diese Funktion startet Threepenny mit Standardkonfiguration und benutzt dabei setup um das Fenster aufzubauen. 

-}
runGUI :: IO () 
runGUI = do startGUI defaultConfig setup

{-

Diese Funktion wird von Threepenny aufgerufen, um das Fenster aufzubauen. 
Sie bekommt ein Window übergeben, in dem sie die GUI-Elemente platzieren kann.

-}

setup :: Window -> UI ()
setup window = do
   void $ return window # set title "Polynom-Parser"    
{- Hier können weitere GUI-Elemente hinzugefügt werden, z.B. Buttons, Textfelder, etc. -}