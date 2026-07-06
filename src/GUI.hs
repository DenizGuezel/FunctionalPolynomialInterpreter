module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)
import qualified Graphics.UI.Threepenny as Ui
import qualified Control.Applicative as GUI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Text.Read (readMaybe)
import Data.Ratio (numerator, denominator)

import Poly
import ParserSimple
import Tree
import Analysis

{- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... -}

{- 

Hier wird ein neuer Datentyp StoredPoly definiert, der dazu dient, ein Polynom zusammen mit einem Namen zu speichern, 
quasi Map-Paar sozusagen, damit ein bestimmter Polynom anhand des Namens abgerufen werden kann.

Wird verwendet, um die Auswahl von Polynomen in der GUI als Liste zu realisieren.

-}

data StoredPoly = StoredPoly String Poly
   deriving (Show, Eq)

{- 

Dieser Datentyp GuiResult wird verwendet, um das Ergebnis einer GUI-Operation zu repräsentieren.
Wir verwenden den Hier für den Darstellungsbereich, StoredPoly hingegen wurde für das Berechnen, bzw. das realisieren der
Operationen in der GUI verwendet.

NoResult: Es gibt kein Ergebnis, z.B. wenn der Benutzer noch keine Operation ausgeführt hat.
PolyResult String Poly: Das Ergebnis ist ein Polynom, zusammen mit einem Namen (String), z.B. bei der Addition von zwei Polynomen (PolyResult "p1 + p2" resultPoly).
ValueResult String Rational: Das Ergebnis ist ein Wert (Rational), zusammen mit einem Namen (String), z.B. bei der Auswertung eines Polynoms an einer bestimmten Stelle (ValueResult "p1(2)" resultValue).
DivResult String Poly Poly: Das Ergebnis ist eine Division von zwei Polynomen, zusammen mit einem Namen (String), z.B. bei der Division von zwei Polynomen (DivResult "p1 / p2" quotientPoly restPoly).

-}

data GuiResult
   = NoResult
   | PolyResult String Poly
   | ValueResult String Rational
   | DivResult String Poly Poly
   deriving (Show, Eq)

{- 

Diese Funktion startet Threepenny mit Standardkonfiguration und benutzt dabei setup um das Fenster aufzubauen. 

-}

runGUI :: IO () 
runGUI = startGUI defaultConfig { jsStatic = Just "static" } setup

{-

Diese Funktion wird von Threepenny aufgerufen, um das Fenster aufzubauen. 
Sie bekommt ein Window übergeben, in dem sie die GUI-Elemente platzieren kann.

Threepenny nutzt im Hintergrund HTML und CSS, z.B. um Buttons, Textfelder, etc. darzustellen.
z.B ist UI.button ein Button gleich zu <button> </button> in HTML.

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

Externe CSS können wir erstellen und anbinden, um das Aussehen der GUI zu verändern.
Das Anbinden tuhen wir in void $ getHead window #+ [ UI.link # set UI.rel "stylesheet" # set UI.href "static/style.css" ]
getHead ist der Head des Fensters, in dem wir die CSS-Datei einbinden können, vergleichbar mit <head> </head> in HTMl.
Ui.link ist dann ein <link> Tag, der innerhalb des <head> Tags platziert wird, um die CSS-Datei einzubinden.
UI.link # set UI.rel "stylesheet" ist gleich zu <link rel="stylesheet" href="static/style.css"> in HTML.

Da wir eine externe CSS-Datei einbinden, und dort .<Klassenname> definieren, können wir sie hier hinter den Elementen 
mit # set UI.class_ "<Klassenname>" ansprechen, um das Aussehen der Elemente zu verändern.

-}

setup :: Window -> UI ()
setup window = do
   void $ return window # set title "Polynom-Parser"   
   {- Hier können weitere GUI-Elemente hinzugefügt werden, z.B. Buttons, Textfelder, etc. -}

   void $ getHead window #+ [ 
      UI.link # set UI.rel "stylesheet"
              # set UI.href "static/style.css"
      ]

   lambdaLogo <- UI.div
      # set UI.text "λ"
      # set UI.class_ "lambda-logo"

   headline <- UI.h1
      # set UI.text "Functional Polynomial Interpreter"
      # set UI.class_ "app-title"

   header <- UI.div
      # set UI.class_ "header"
      #+ [element lambdaLogo, element headline]

   {- Eingabefelder: -}

   input <- UI.input # set (attr "placeholder") "Polynom hinzufügen"
   inputX <- UI.input # set (attr "placeholder") "x-Wert"

   {- Operation-Buttons: -}

   buttonnormalize <- UI.button
      # set UI.html "<span class='button-symbol'>N</span><span>Normalisieren</span>"
      # set UI.class_ "operation-button"

   buttonnegat <- UI.button
      # set UI.html "<span class='button-symbol'>−p</span><span>Negieren</span>"
      # set UI.class_ "operation-button"

   buttonaddpoly <- UI.button
      # set UI.html "<span class='button-symbol'>+</span><span>Polynom hinzufügen</span>"
      # set UI.class_ "operation-button"

   buttonadd <- UI.button
      # set UI.html "<span class='button-symbol'>+</span><span>Addieren</span>"
      # set UI.class_ "operation-button"

   buttonsub <- UI.button
      # set UI.html "<span class='button-symbol'>−</span><span>Subtrahieren</span>"
      # set UI.class_ "operation-button"

   buttonmult <- UI.button
      # set UI.html "<span class='button-symbol'>×</span><span>Multiplizieren</span>"
      # set UI.class_ "operation-button"

   buttonderivation <- UI.button
      # set UI.html "<span class='button-symbol'>d/dx</span><span>Ableiten</span>"
      # set UI.class_ "operation-button"

   buttonevaluate <- UI.button
      # set UI.html "<span class='button-symbol'>f(x)</span><span>Auswerten</span>"
      # set UI.class_ "operation-button"

   buttondiv <- UI.button
      # set UI.html "<span class='button-symbol'>÷</span><span>Dividieren</span>"
      # set UI.class_ "operation-button"

   {- Darstellung-Buttons: -}
   buttonshowresult <- UI.button
      # set UI.html "<span class='button-symbol'>i</span><span>Ergebnis</span>"
      # set UI.class_ "view-button"

   buttonshowlatex <- UI.button
      # set UI.html "<span class='button-symbol'>TeX</span><span>LaTeX</span>"
      # set UI.class_ "view-button"

   buttontree <- UI.button
      # set UI.html "<span class='button-symbol'>🌳</span><span>Baum</span>"
      # set UI.class_ "view-button"

   buttonanalysis <- UI.button
      # set UI.html "<span class='button-symbol'>📊</span><span>Analyse</span>"
      # set UI.class_ "view-button"

   {- Speicher: -}

   polyStore <- liftIO $ newIORef ([] :: [StoredPoly]) --Für die Speicherung der Polynome
   resultStore <- liftIO $ newIORef NoResult --Für die Speicherung der Ergebnisse der Operationen

   {- Ausgabebereiche: -}

   output <- UI.pre # set UI.text ""
   polyListOutput <- UI.pre # set UI.text "Noch keine Polynome vorhanden."

   getBody window #+ [

      element header,
      element input,
      element inputX,
      element buttonnormalize,
      element buttonnegat,
      element buttonaddpoly,
      element buttonadd,
      element buttonsub,
      element buttonmult,
      element buttonderivation,
      element buttonevaluate,
      element buttondiv,
      element buttonshowresult,
      element buttonshowlatex,
      element buttontree,
      element buttonanalysis,
      element polyListOutput,
      element output
      
      ] 

   {- ActionListener auf die Operation-Buttons: -}

   on UI.click buttonnormalize (\_ -> handlenormalizeclick input resultStore output) 
   on UI.click buttonnegat (\_ -> handlenegatclick input resultStore output)
   on UI.click buttonaddpoly (\_ -> handleaddpolyclick input polyListOutput polyStore)
   on UI.click buttonadd (\_ -> handleaddclick polyStore resultStore output)
   on UI.click buttonsub (\_ -> handlesubclick polyStore resultStore output)
   on UI.click buttonmult (\_ -> handlemultclick polyStore resultStore output)
   on UI.click buttonderivation (\_ -> handlederivationclick polyStore resultStore output)
   on UI.click buttonevaluate (\_ -> handleevaluateclick polyStore inputX resultStore output)
   on UI.click buttondiv (\_ -> handledivclick polyStore resultStore output)

   {- ActionListener auf die Darstellung-Bbuttons: -}

   on UI.click buttonshowresult (\_ -> handleshowresultclick resultStore output)
   on UI.click buttonshowlatex (\_ -> handlelatexclick resultStore output)
   on UI.click buttontree (\_ -> handletreeclick resultStore output)
   on UI.click buttonanalysis (\_ -> handleanalysisclick resultStore output)

{- 

Diese Funktion dient zur Veranschaulichung des normalisierten Polynoms in der GUI.

die Funktion wird aufgerufen, wenn der Button geklickt wird.
Sie bekommt das Eingabefeld (String), resultStore und den Ausgabebereich übergeben.

Das Eingabefeld brauchen wir, um den eingegebenen String auszulesen.
resultStore brauchen wir, um das Ergebnis der Operation abzuspeichern, damit die Darstellungsbuttons wie Ergebnis und LaTeX später darauf zugreifen können.
output brauchen wir, um das Ergebnis direkt in der GUI anzuzeigen.

polyStr <- get value input liest den Wert aus dem Eingabefeld aus und speichert ihn in polyStr.

Dann wird parsePolySimple auf polyStr ausgeführt, um das Polynom zu parsen und das Parsergebnis wird in "result" abgespeichert.
Wenn das Parsergebnis ein Fehler ist (Left err), wird der Fehler im Ausgabebereich angezeigt.

Wenn das Parsergebnis ein gültiges Polynom ist (Right poly), wird normalize auf das Polynom angewendet.
Das normalisierte Polynom speichern wir in resultPoly.

Danach wird resultPoly mit writeIORef in resultStore gespeichert.
Dabei verwenden wir PolyResult, weil normalize wieder ein Polynom zurückgibt.

Am Ende wird das Ergebnis mit toPrettyMathPoly schön mathematisch in der GUI angezeigt.

Mit void $ sagen wir, dass wir den Rückgabewert der Funktion ignorieren. Das machen wir weil 
set UI.text einen Ui.Element zurückgibt, den wir hier aber nicht benötigen.

-}

handlenormalizeclick :: Element -> IORef GuiResult -> Element -> UI ()
handlenormalizeclick input resultStore output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr 
   case result of
      Left err ->  void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         let resultPoly = normalize poly
         liftIO $ writeIORef resultStore (PolyResult "Normalisieren" resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)
{- 

Diese Funktion dient zur Veranschaulichung eines negierten Polynoms in der GUI.

Gleiche Logik wie bei handlenormalizeclick, nur dass hier die Funktion negat aufgerufen wird, um das Polynom zu negieren.

Auch hier wird das Ergebnis zusätzlich in resultStore gespeichert.
Das ist wichtig, damit man danach z.B. auf den LaTeX-Button klicken kann, ohne dass nochmal neu gerechnet werden muss.

Da negat wieder ein Polynom zurückgibt, speichern wir das Ergebnis als PolyResult.

-}

handlenegatclick :: Element -> IORef GuiResult -> Element -> UI ()
handlenegatclick input resultStore output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->  void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         let resultPoly = negat poly
         liftIO $ writeIORef resultStore (PolyResult "Negieren" resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)

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

Es wird ein Polynom im folgenden Format zum hinzufügen eingegeben: "3 2; 2 1; 1 0" (Koeffizient Exponent; Koeffizient Exponent; Koeffizient Exponent)
Es wird ein Name automatisch generiert, z.B. p1, p2, p3 usw. und das Polynom wird in der GUI angezeigt (z.B. p1 = 3x^2 + 2x + 1).

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

{- 

Diese Funktion wandelt die gespeicherten Polynome in einen String um,
damit wir sie erstmal einfach in der GUI anzeigen können.

Wenn die Liste leer ist, wird angezeigt, dass noch keine Polynome vorhanden sind.

Wenn mindestens ein StoredPoly vorhanden ist (mindestens ein Element in der Liste, z.b. [StoredPoly "p1" poly1]),
wird die Hilfsfunktion showStoredPolysRec aufgerufen.


-}

showStoredPolys :: [StoredPoly] -> String
showStoredPolys [] = "Noch keine Polynome vorhanden."
showStoredPolys xs = showStoredPolysRec xs

{- 

Die Hilfsfunktion showStoredPolysRec wird rekursiv aufgerufen, um die gespeicherten Polynome in einen String umzuwandeln.

Falls die übergebene Liste leer ist, wird ein leerer String zurückgegeben.
Ansonsten wird das erste StoredPoly aus der Liste genommen und in einen String umgewandelt, 
den wir dann mit dem Ergebnis der rekursiven Aufrufe auf den Rest der Liste verketten.

-}

showStoredPolysRec :: [StoredPoly] -> String
showStoredPolysRec [] = ""
showStoredPolysRec (StoredPoly name poly : rest) = name ++ " = " ++ toLaTeX poly ++ "\n" ++ showStoredPolysRec rest

{- 

Diese Funktion dient zur Veranschaulichung der Addition von zwei Polynomen in der GUI. (Professionellere Version kommt später, 
da wir die checkboxen und Listenaktualisierung noch brauchen)

Wir übergeben polyStore, resultStore und output.

polyStore brauchen wir, um die gespeicherten Polynome auszulesen.
resultStore brauchen wir, um das Ergebnis der Addition zu speichern.
output brauchen wir, um das Ergebnis direkt in der GUI anzuzeigen.

Wir erstellen storedPolys, um die gespeicherten Polynome aus polyStore zu lesen.

Wir dürfen nicht (StoredPoly name1 poly1 : StoredPoly name2 poly2 : rest) schreiben, da wir sonst die anderen Fallprüfungen
garnicht erreichen, da wir damit checken würden, dass MINDESTENS zwei Polynome gespeichert sind, aber wir wollen ja auch die Fälle abfangen, 
dass gar keins gespeichert ist (oder nur eins oder mehrere -> Andernfalls).

Es entstehen drei Fälle, beim Lesen der gespeicherten Polynome:
Fall 1: Es sind genau zwei Polynome gespeichert, dann können wir die Addition durchführen.
Fall 2: Es sind keine Polynome gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 3 (Andernfalls): Es ist nur ein Polynom gespeichert oder mehrere, dann wird ebenfalls eine Fehlermeldung angezeigt.

Wenn genau zwei Polynome vorhanden sind, wird add poly1 poly2 ausgeführt.
Das Ergebnis speichern wir in resultPoly.

Danach speichern wir resultPoly mit writeIORef in resultStore.
Da add wieder ein Polynom zurückgibt, speichern wir das Ergebnis als PolyResult.

-}

handleaddclick :: IORef [StoredPoly] -> IORef GuiResult -> Element -> UI ()
handleaddclick polyStore resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> do
         let resultPoly = add poly1 poly2
         liftIO $ writeIORef resultStore (PolyResult (name1 ++ " + " ++ name2) resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)

      [] -> void $ element output # set UI.text "Fehler: Addieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert." 

      other -> void $ element output # set UI.text ("Fehler: Addieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung der Subtraktion von zwei Polynomen in der GUI.

Funktioniert genau wie handleaddclick, nur dass hier die Funktion sub aufgerufen wird, um die Subtraktion durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da sub wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handlesubclick :: IORef [StoredPoly] -> IORef GuiResult -> Element -> UI ()
handlesubclick polyStore resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> do
         let resultPoly = sub poly1 poly2
         liftIO $ writeIORef resultStore (PolyResult (name1 ++ " - " ++ name2) resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)

      [] -> void $ element output # set UI.text "Fehler: Subtrahieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Subtrahieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Multiplikation von zwei Polynomen in der GUI.

Funktioniert genau wie handleaddclick, nur dass hier die Funktion mult aufgerufen wird, um die Multiplikation durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da mult wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handlemultclick :: IORef [StoredPoly] -> IORef GuiResult -> Element -> UI ()
handlemultclick polyStore resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> do
         let resultPoly = mult poly1 poly2
         liftIO $ writeIORef resultStore (PolyResult (name1 ++ " * " ++ name2) resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)

      [] -> void $ element output # set UI.text "Fehler: Multiplizieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Multiplizieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Ableitung eines Polynoms in der GUI.

Wir übergeben als Parameter polyStore, resultStore und output.

polyStore brauchen wir, um die gespeicherten Polynome zu lesen.
resultStore brauchen wir, um das Ergebnis der Ableitung zu speichern.
output brauchen wir, um das Ergebnis direkt anzuzeigen.

Anders als bei der Addition, Subtraktion und Multiplikation, benötigen wir hier nur ein Polynom, um die Ableitung durchzuführen.

Wir lesen die gespeicherten Polynome aus polyStore und speichern sie in storedPolys.

Danach prüfen wir 3 Fälle:
Fall 1: Es ist ein Polynom gespeichert, dann können wir die Ableitung durchführen.
Fall 2: Es ist kein Polynom gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 3 (Andernfalls): Es sind zwei Polynome gespeichert, oder mehr, dann wird ebenfalls eine Fehlermeldung angezeigt.

Wenn genau ein Polynom vorhanden ist, wird derivation poly1 ausgeführt.
Das Ergebnis speichern wir in resultPoly.

Danach speichern wir resultPoly mit writeIORef in resultStore.
Da derivation wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handlederivationclick :: IORef [StoredPoly] -> IORef GuiResult -> Element -> UI ()
handlederivationclick polyStore resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of

      [StoredPoly name1 poly1] -> do
         let resultPoly = derivation poly1
         liftIO $ writeIORef resultStore (PolyResult (name1 ++ "'") resultPoly)
         void $ element output # set UI.text ("Ergebnis: " ++ toPrettyMathPoly resultPoly)

      [] -> void $ element output # set UI.text "Fehler: Ableiten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Ableiten benötigt ein Polynom. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Auswertung eines Polynoms an einer bestimmten Stelle in der GUI.

Es wird das polyStore übergeben, um die gespeicherten Polynome zu lesen, das input Element, 
um den Wert für x auszulesen, resultStore, um das Ergebnis zu speichern und das output Element, um das Ergebnis der Auswertung anzuzeigen.

Wir lesen die gespeicherten Polynome aus polyStore und speichern sie in storedPolys.
Wir lesen den Wert für x aus dem input Element aus und speichern ihn in xStr.

Danach prüfen wir für xStr 3 Fälle:
Fall 1: xStr ist leer, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Eingabefeld für xStr ist nicht leer, 
somit gehen wir über in die Prüfung ob es sich um eine gültige Zahl handelt, mithilfe von readMaybe, 
das besagt, dass eine Rational Zahl eingelesen werden kann (Just x) oder nicht (Nothing):

Fall 2.1: Es handelt sich nicht um eine gültige Zahl, dann wird eine Fehlermeldung angezeigt.
Fall 2.2: Es handelt sich um eine gültige Zahl, dann prüfen wir die gespeicherten Polynome:

Fall 2.2.1: Es ist ein Polynom gespeichert, dann wird die Auswertung durchgeführt und das Ergebnis angezeigt.
Fall 2.2.2: Es ist kein Polynom gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 2.2.3 (Andernfalls): Es sind zwei Polynome gespeichert, oder mehr, dann wird ebenfalls eine Fehlermeldung angezeigt.

Wenn genau ein Polynom und ein gültiger x-Wert vorhanden sind, wird evaluate poly1 x ausgeführt.
Das Ergebnis ist dann kein Polynom, sondern ein Rational-Wert.

Deshalb speichern wir das Ergebnis in resultStore als ValueResult.

-}

handleevaluateclick :: IORef [StoredPoly] -> Element -> IORef GuiResult -> Element -> UI ()
handleevaluateclick polyStore input resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   xStr <- get value input
   case xStr of
      "" -> void $ element output # set UI.text "Fehler: Bitte geben Sie einen Wert für x ein."
      other -> case readMaybe xStr :: Maybe Rational of
         Nothing -> void $ element output # set UI.text "Fehler: Bitte geben Sie eine gültige Zahl für x ein."
         Just x -> case storedPolys of
            [StoredPoly name1 poly1] -> do
               let resultValue = evaluate poly1 x
               liftIO $ writeIORef resultStore (ValueResult (name1 ++ "(" ++ show x ++ ")") resultValue)
               void $ element output # set UI.text ("Ergebnis: " ++ prettyRational resultValue)

            [] -> void $ element output # set UI.text "Fehler: Auswerten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."

            other -> void $ element output # set UI.text ("Fehler: Auswerten benötigt ein Polynom. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung der Division von zwei Polynomen in der GUI.

Wird im Grunde genau so wie die Addition, Subtraktion und Multiplikation gehandhabt, 
nur dass wir hier die toLaTeX Funktion nicht auf ein (Poly,Poly) anwenden können und 
wir daher die Division in zwei Teile aufteilen müssen, nämlich den Quotienten und den Rest.

Somit können wir die Division von zwei Polynomen in der GUI sauber darstellen, indem wir den Quotienten und den Rest getrennt anzeigen.

Das Ergebnis wird außerdem in resultStore gespeichert.
Da Division nicht nur ein einzelnes Polynom zurückgibt, benutzen wir hier DivResult.
DivResult speichert den Namen der Operation, den Quotienten und den Rest.

-}

handledivclick :: IORef [StoredPoly] -> IORef GuiResult -> Element -> UI ()
handledivclick polyStore resultStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of 
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> do
         let (quotient, rest) = (/%) poly1 poly2
         liftIO $ writeIORef resultStore (DivResult (name1 ++ " / " ++ name2) quotient rest)
         void $ element output # set UI.text ("Ergebnis: " ++ name1 ++ " / " ++ name2 ++ " = " ++ toPrettyMathPoly quotient ++ ", Rest: " ++ toPrettyMathPoly rest)

      [] -> void $ element output # set UI.text "Fehler: Dividieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Dividieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion errechnet nichts neu, sondern zeigt das Ergebnis als LaTeX in der GUI an, wenn der Button "LaTeX" geklickt wird.
Wir lesen das Ergebnis einer Berechnung aus resultStore aus und prüfen vier Fälle:

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann wird das Polynom in LaTeX angezeigt.
Fall 3: Das Ergebnis ist ein Wert, dann wird der Wert in LaTeX angezeigt
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann wird der Quotient und der Rest in LaTeX angezeigt.

-}

handlelatexclick :: IORef GuiResult -> Element -> UI ()
handlelatexclick resultStore output = do
   result <- liftIO $ readIORef resultStore

   case result of
      NoResult ->
         void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."

      PolyResult name poly ->
         void $ element output # set UI.text
            ("LaTeX von " ++ name ++ ": " ++ toLaTeX poly)

      ValueResult name value ->
         void $ element output # set UI.text
            ("LaTeX von " ++ name ++ ": " ++ toLaTeX value)

      DivResult name quotient rest ->
         void $ element output # set UI.text
            ("LaTeX von " ++ name ++ ": Quotient = "
             ++ toLaTeX quotient ++ ", Rest = " ++ toLaTeX rest)


{- 

Diese Funktion wird aufgerufen, wenn der Button "Ergebnis" geklickt wird.
Sie dient dazu, das Ergebnis einer Berechnung mathematisch anzuzeigen.

GLeiches Vorgehen wie bei handlelatexclick, nur dass hier die mathematische Darstellung (toPrettyMathPoly und toPrettyMathRational) verwendet wird,
um das Ergebnis in einer mathematischen Form anzuzeigen, die für den Benutzer leichter verständlich ist.

-}

handleshowresultclick :: IORef GuiResult -> Element -> UI ()
handleshowresultclick resultStore output = do
   result <- liftIO $ readIORef resultStore

   case result of
      NoResult ->
         void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."

      PolyResult name poly ->
         void $ element output # set UI.text
            ("Ergebnis von " ++ name ++ ": " ++ toPrettyMathPoly poly)

      ValueResult name value ->
         void $ element output # set UI.text
            ("Ergebnis von " ++ name ++ ": " ++ prettyRational value)

      DivResult name quotient rest ->
         void $ element output # set UI.text
            ("Ergebnis von " ++ name ++ ": Quotient = "
             ++ toPrettyMathPoly quotient ++ ", Rest = " ++ toPrettyMathPoly rest)

{- 

Diese Funktionen dienen dazu, Polynome und rationale Zahlen in einer mathematischen Form darzustellen, 
die für den Benutzer leichter verständlich ist.

Die Hauptfunktion toPrettyMathPoly (welche auch eine Hilfsfunktion eigentlich für handleShowResultClick ist) ruft die Hilfsfunktion prettyPoly auf, 
um das Polynom in eine mathematische Form zu bringen.

prettyPoly ruft wiederum die Hilfsfunktionen prettyMonomFirst und prettyMonomRest auf, 
um die einzelnen Monome des Polynoms in eine mathematische Form zu bringen.

Genau so geht es weiter, bis die kleinste Einheit, nämlich die Koeffizienten und Exponenten, in eine mathematische Form gebracht werden.

-}

toPrettyMathPoly :: Poly -> String
toPrettyMathPoly p = prettyPoly (normalize p)

{- Wandelt ein Polynom in eine saubere mathematische Form um-}
prettyPoly :: Poly -> String
prettyPoly (P []) = "0"
prettyPoly (P (m:ms)) = prettyMonomFirst m ++ prettyMonomRest ms

{- Wandelt eine Monomliste in eine saubere mathematische Form um -}
prettyMonomRest :: [Monom] -> String
prettyMonomRest [] = ""
prettyMonomRest (m:ms) = prettyMonomWithSign m ++ prettyMonomRest ms

{- - Wandelt ein Monom in eine saubere mathematische Form um inklusive der Vorzeichen-}
prettyMonomWithSign :: Monom -> String
prettyMonomWithSign (M k e)
   | k >= 0 =
      " + " ++ prettyMonom (M k e)
   | otherwise =
      " - " ++ prettyMonom (M (-k) e)

{- Wandelt das erste Monom in eine saubere mathematische Form um, ohne Vorzeichen davor -}
prettyMonomFirst :: Monom -> String
prettyMonomFirst (M k e)
   | k < 0 =
      "-" ++ prettyMonom (M (-k) e)
   | otherwise =
      prettyMonom (M k e)

{- Wandelt ein komplettes Monom in eine saubere mathematische Form um -}
prettyMonom :: Monom -> String
prettyMonom (M k 0) = prettyRational k
prettyMonom (M k 1)
   | k == 1 =
      "x"
   | otherwise =
      prettyRational k ++ "x"
prettyMonom (M k e)
   | k == 1 =
      "x" ++ prettyExponent e
   | otherwise =
      prettyRational k ++ "x" ++ prettyExponent e

{- 

Diese Funktion wird aufgerufen, wenn der Button "Baum" geklickt wird.
Sie dient dazu, das Ergebnis einer Berechnung in Form eines Baumes anzuzeigen.

Die Funktion bekommt die aktuellen Ergebnisse aus resultStore und den Ausgabebereich output übergeben.
Danach wird geprüft, ob es ein Ergebnis gibt oder nicht, indem das Ergebnis auf vier Fälle überprüft wird.

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann wird das Polynom in einen Baum umgewandelt und angezeigt.
Fall 3: Das Ergebnis ist ein Wert, dann wird der Wert in einen Baum umgewandelt und angezeigt.
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann wird der Quotient und der Rest in einen Baum umgewandelt und angezeigt.

-}

handletreeclick :: IORef GuiResult -> Element -> UI ()
handletreeclick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult -> void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult name poly -> do
         let tree = polyToExprTree poly
         void $ element output # set UI.text ("Baum von " ++ name ++ ":\n" ++ prettyTree tree)
      ValueResult name value -> do
         let tree = TConst value
         void $ element output # set UI.text ("Baum von " ++ name ++ ":\n" ++ prettyTree tree)
      DivResult name quotient rest -> do
         let treequotient = polyToExprTree quotient
         let treerest = polyToExprTree rest
         void $ element output # set UI.text ("Baum von " ++ name ++ ":\nQuotient:\n" ++ prettyTree treequotient ++ "\nRest:\n" ++ prettyTree treerest)

{- 

Diese Funktion wird aufgerufen, wenn der Button "Analyse" geklickt wird.
Sie dient dazu, die Analyse eines Baumes anzuzeigen.

Die Funktion bekommt die aktuellen Ergebnisse aus resultStore und den Ausgabebereich output übergeben.
Danach wird geprüft, ob es ein Ergebnis gibt oder nicht, indem das Ergebnis auf vier Fälle überprüft wird.

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann wird das Polynom in einen Baum umgewandelt und analysiert.
Fall 3: Das Ergebnis ist ein Wert, dann wird der Wert in einen Baum umgewandelt und analysiert.
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann wird der Quotient und der Rest in einen Baum umgewandelt und analysiert.

-}

handleanalysisclick :: IORef GuiResult -> Element -> UI ()
handleanalysisclick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult -> void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult name poly -> do
         let analysis = analyzeTree (polyToExprTree poly)
         void $ element output # set UI.text ("Analyse von Baum" ++ name ++ ":\n" ++ analysis)
      ValueResult name value -> do
         let analysis = analyzeTree (TConst value)
         void $ element output # set UI.text ("Analyse von Baum " ++ name ++ ":\n" ++ analysis)
      DivResult name quotient rest -> do
         let analysisQuotient = analyzeTree (polyToExprTree quotient)
         let analysisRest = analyzeTree (polyToExprTree rest)
         void $ element output # set UI.text ("Analyse von Baum " ++ name ++ ":\nQuotient:\n" ++ analysisQuotient ++ "\nRest:\n" ++ analysisRest)
