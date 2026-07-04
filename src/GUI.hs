module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)

import Poly
import ParserSimple
import qualified Graphics.UI.Threepenny as Ui
import qualified Control.Applicative as GUI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
-- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... --

{- 

Hier wird ein neuer Datentyp StoredPoly definiert, der dazu dient, ein Polynom zusammen mit einem Namen zu speichern, 
quasi Map-Paar sozusagen, damit ein bestimmter Polynom anhand des Namens abgerufen werden kann.

Wird verwendet, um die Auswahl von Polynomen in der GUI als Liste zu realisieren.

-}

data StoredPoly = StoredPoly String Poly
   deriving (Show, Eq)


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

Mit on UI click ... definieren wir eine Funktion, die aufgerufen wird, 
wenn der Button geklickt wird, vergleichbar mit einem ActionListener in Java.

-}

setup :: Window -> UI ()
setup window = do
   void $ return window # set title "Polynom-Parser"   
   {- Hier können weitere GUI-Elemente hinzugefügt werden, z.B. Buttons, Textfelder, etc. -}

   headline <- UI.h1 # set UI.text "Functional Polynomial Parser"

   {- Eingabefelder: -}

   input <- UI.input # set (attr "placeholder") "Polynom hinzufügen"
   inputX <- UI.input # set (attr "placeholder") "x-Wert"

   {- Buttons: -}
   buttonnormalize <- UI.button # set UI.text "Normalisieren"
   buttonnegat <- UI.button # set UI.text "Negieren"
   buttonadd <- UI.button # set UI.text "Addieren"
   
   {- Speicher für gespeicherte Polynome: -}
   polyStore <- liftIO $ newIORef ([] :: [StoredPoly])

   {- Ausgabebereiche: -}
   output <- UI.div # set UI.text ""
   polyListOutput <- UI.div # set UI.text "Noch keine Polynome vorhanden."


   getBody window #+ [

      element headline, 
      element input, 
      element inputX, 
      element buttonnormalize, 
      element buttonnegat, 
      element output
      
      ] 

   {- ActionListener auf die Buttons: -}
   on UI.click buttonnormalize (\_ -> handlenormalizeclick input output) 
   on UI.click buttonnegat (\_ -> handlenegatclick input output)
{- 

Diese Funktion dient zur Veranschaulichung des normalisierten Polynoms in der GUI.

die Funktion wird aufgerufen, wenn der Button geklickt wird.
Sie bekommt das Eingabefeld (String) und den Ausgabebereich (wenn Parsen fehlschlägt ein Fehler, 
ansonsten das normalisierte Polynom) übergeben, um das Ergebnis des Parsens anzuzeigen.

polyStr <- get value input liest den Wert aus dem Eingabefeld aus und speichert ihn in polyStr.

Dann wird parsePolySimple auf polyStr ausgeführt, um das Polynom zu parsen und das Parsergebnis wird in "result" abgespeichert.
Wenn das Parsergebnis ein Fehler ist (Left err), wird der Fehler im Ausgabebereich angezeigt.
Wenn das Parsergebnis ein gültiges Polynom ist (Right poly), wird das Polynom im Ausgabebereich normalisiert angezeigt.

Mit void $ sagen wir, dass wir den Rückgabewert der Funktion ignorieren. Das machen wir weil 
set UI.text einen Ui.Element zurückgibt, den wir hier aber nicht benötigen.

-}

handlenormalizeclick :: Element -> Element -> UI ()
handlenormalizeclick input output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr 
   case result of
      Left err ->  void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> void $ element output # set UI.text ("Ergebnis: " ++ show (normalize poly))

{- 

Diese Funktion dient zur Veranschaulichung eines negierten Polynoms in der GUI.
Gleiche Logik wie bei handlenormalizeclick, nur dass hier die Funktion negat aufgerufen wird, um das Polynom zu negieren.

-}

handlenegatclick :: Element -> Element -> UI ()
handlenegatclick input output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->  void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> void $ element output # set UI.text ("Ergebnis: " ++ show (negat poly))

{- 

Diese Funktion wird aufgerufen, wenn der Button "Polynom hinzufügen" geklickt wird.

Sie liest zuerst den Text aus dem Eingabefeld aus.

Danach wird mit parsePolySimple versucht, aus diesem String ein echtes Polynom zu machen.

Wenn das Parsen fehlschlägt, entsteht ein Left err und der Fehler wird in der Polynomliste angezeigt.

Wenn das Parsen klappt, entsteht ein Right poly.
Dann wird die bisher gespeicherte Polynomliste aus polyStore geholt.

Anschließend erzeugen wir automatisch einen Namen, z.B. p1, p2, p3 usw.
Dazu nehmen wir die Länge der bisherigen Liste und rechnen + 1.

Dann wird aus dem Namen und dem Polynom ein StoredPoly gebaut.

Dieses neue gespeicherte Polynom wird hinten an die bisherige Liste angehängt.
Danach wird die neue Liste wieder in polyStore gespeichert.

Am Ende wird die sichtbare Polynomliste in der GUI aktualisiert.

-}

handleaddpolyclick :: Element -> Element -> IORef [StoredPoly] -> UI ()
handleaddpolyclick input polyListOutput polyStore = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->
         void $ element polyListOutput # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         storedPolys <- liftIO $ readIORef polyStore
         let name = "p" ++ show (length storedPolys + 1)
         let newPoly = StoredPoly name poly
         let newStoredPolys = storedPolys ++ [newPoly]
         liftIO $ writeIORef polyStore newStoredPolys
         void $ element polyListOutput # set UI.text (showStoredPolys newStoredPolys)
