module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)

import Poly
import ParserSimple
import qualified Graphics.UI.Threepenny as Ui

{- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... -}


{- 

Diese Funktion startet Threepenny mit Standardkonfiguration und benutzt dabei setup um das Fenster aufzubauen. 

-}

runGUI :: IO () 
runGUI = do startGUI defaultConfig setup

{-

Diese Funktion wird von Threepenny aufgerufen, um das Fenster aufzubauen. 
Sie bekommt ein Window übergeben, in dem sie die GUI-Elemente platzieren kann.

Threepenny nutzt im Hintergrund HTML und CSS, z.B. um Buttons, Textfelder, etc. darzustellen.
z.B ist UI.button ein Button gleich zu <button> </button> in HTML.

Externe CSS können wir erstellen und anbinden, um das Aussehen der GUI zu verändern.

Mit void $ return window # set title "Polynom-Parser" setzen wir den Fenstertitel.

Mit headline definieren wir eine Überschrift, die wir mit UI.h1 erstellen, welche als Überschrift dient.
Mit input definieren wir ein Eingabefeld, in das der Benutzer ein Polynom eingeben kann.
Mit button definieren wir einen Button, der zum Parsen des Polynoms verwendet werden kann.
Mit output definieren wir einen Bereich (div), in dem das Ergebnis des Parsens angezeigt werden kann.

wir definieren jeweils vor dem <- den Namen des Elements, nach dem <- sagen wir erst, um welches Element es sich handelt (z.B U1.h1 ist in html <h1> </h1>), 
danach mit # können wir Eigenschaften des Elements setzen, z.B. den Text, der angezeigt werden soll.

Mit getBody window bekommen wir den Body des Fensters, in dem wir die Elemente platzieren können.

Mit on UI click button (\_ -> handleclick input output) definieren wir eine Funktion, die aufgerufen wird, 
wenn der Button geklickt wird, vergleichbar mit einem ActionListener in Java.

-}

setup :: Window -> UI ()
setup window = do
   void $ return window # set title "Polynom-Parser"   
{- Hier können weitere GUI-Elemente hinzugefügt werden, z.B. Buttons, Textfelder, etc. -}

   headline <- UI.h1 # set UI.text "Functional Polynomial Parser"

   input <- UI.input # set (attr "placeholder") "Geben Sie ein Polynom ein"

   button <- UI.button # set UI.text "Parse"

   output <- UI.div # set UI.text ""

   getBody window #+ [element headline, element input, element button, element output] 

   on UI.click button (\_ -> handleclick input output) 


{- 

Funktion, die aufgerufen wird, wenn der Button geklickt wird.
Sie bekommt das Eingabefeld und den Ausgabebereich übergeben, um das Ergebnis des Parsens anzuzeigen.

polyStr <- get value input liest den Wert aus dem Eingabefeld aus und speichert ihn in polyStr.

Dann wird parsePolySimple auf polyStr ausgeführt, um das Polynom zu parsen.
Wenn das Ergebnis ein Fehler ist (Left err), wird der Fehler im Ausgabebereich angezeigt.
Wenn das Ergebnis ein gültiges Polynom ist (Right poly), wird das Polynom im Ausgabebereich angezeigt.

Mit void $ sagen wir, dass wir den Rückgabewert der Funktion ignorieren. Das machen wir weil 
set UI.text einen Ui.Element zurückgibt, den wir hier aber nicht benötigen.

-}

handleclick :: Element -> Element -> UI ()
handleclick input output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->  void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> void $ element output # set UI.text ("Ergebnis: " ++ show poly)
