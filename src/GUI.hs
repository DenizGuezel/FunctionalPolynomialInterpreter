module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)

import Poly
import ParserSimple
import qualified Graphics.UI.Threepenny as Ui
import qualified Control.Applicative as GUI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Text.Read (readMaybe)
import Data.Ratio (numerator, denominator)
-- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... --

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

   {- Speicher: -}

   polyStore <- liftIO $ newIORef ([] :: [StoredPoly]) --Für die Speicherung der Polynome
   resultStore <- liftIO $ newIORef NoResult --Für die Speicherung der Ergebnisse der Operationen

   {- Ausgabebereiche: -}

   output <- UI.div # set UI.text ""
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
      element polyListOutput,
      element output
      
      ] 

   {- ActionListener auf die Operation-Buttons: -}

   on UI.click buttonnormalize (\_ -> handlenormalizeclick input resultStore output) 
   on UI.click buttonnegat (\_ -> handlenegatclick input resultStore output)
   on UI.click buttonadd (\_ -> handleaddclick polyStore resultStore output)
   on UI.click buttonsub (\_ -> handlesubclick polyStore resultStore output)
   on UI.click buttonmult (\_ -> handlemultclick polyStore resultStore output)
   on UI.click buttonderivation (\_ -> handlederivationclick polyStore resultStore output)
   on UI.click buttonevaluate (\_ -> handleevaluateclick polyStore inputX resultStore output)
   on UI.click buttondiv (\_ -> handledivclick polyStore resultStore output)

   {- ActionListener auf die Darstellung-Bbuttons: -}

   on UI.click buttonshowresult (\_ -> handleshowresultclick resultStore output)
   on UI.click buttonshowlatex (\_ -> handlelatexclick resultStore output)
   

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

Wir erstellen storedPolys, um die gespeicherten Polynome aus polyStore zu lesen.

Wir dürfen nicht (StoredPoly name1 poly1 : StoredPoly name2 poly2 : rest) schreiben, da wir sonst die anderen Fallprüfungen
garnicht erreichen, da wir damit checken würden, dass MINDESTENS zwei Polynome gespeichert sind, aber wir wollen ja auch die Fälle abfangen, 
dass gar keins gespeichert ist (oder nur eins oder mehrere -> Andernfalls).

Es entstehen drei Fälle, beim Lesen der gespeicherten Polynome:
Fall 1: Es sind mindestens zwei Polynome gespeichert, dann können wir die Addition durchführen.
Fall 2: Es sind keine Polynome gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 3 (Andernfalls): Es ist nur ein Polynom gespeichert oder mehrere, dann wird ebenfalls eine Fehlermeldung angezeigt.

-}

handleaddclick :: IORef [StoredPoly] -> Element -> UI ()
handleaddclick polyStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] ->void $ element output # set UI.text ("Ergebnis: " ++ toLaTeX (add poly1 poly2))

      [] -> void $ element output # set UI.text "Fehler: Addieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert." 

      other -> void $ element output # set UI.text ("Fehler: Addieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung der Subtraktion von zwei Polynomen in der GUI.

Funktionier genau wie handleaddclick, nur dass hier die Funktion sub aufgerufen wird, um die Subtraktion durchzuführen.

-}

handlesubclick :: IORef [StoredPoly] -> Element -> UI ()
handlesubclick polyStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> void $ element output # set UI.text ("Ergebnis: " ++ toLaTeX (sub poly1 poly2))

      [] -> void $ element output # set UI.text "Fehler: Subtrahieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Subtrahieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Multiplikation von zwei Polynomen in der GUI.

Funktionier genau wie handleaddclick, nur dass hier die Funktion mult aufgerufen wird, um die Multiplikation durchzuführen.

-}

handlemultclick :: IORef [StoredPoly] -> Element -> UI ()
handlemultclick polyStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> void $ element output # set UI.text ("Ergebnis: " ++ toLaTeX (mult poly1 poly2))

      [] -> void $ element output # set UI.text "Fehler: Multiplizieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Multiplizieren benötigt zwei Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Ableitung eines Polynoms in der GUI.

Wir übergeben als Parameter polyStore, um die gespeicherten Polynome zu lesen und output, um das Ergebnis der Ableitung anzuzeigen.

Anders als bei der Addition, Subtraktion und Multiplikation, benötigen wir hier nur ein Polynom, um die Ableitung durchzuführen.

Wir lesen die gespeicherten Polynome aus polyStore und speichern sie in storedPolys.

Danach prüfen wir 3 Fälle:
Fall 1: Es ist ein Polynom gespeichert, dann können wir die Ableitung durchführen
Fall 2: Es ist kein Polynom gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 3 (Andernfalls): Es sind zwei Polynome gespeichert, oder mehr, dann wird ebenfalls eine Fehlermeldung angezeigt.

-}

handlederivationclick :: IORef [StoredPoly] -> Element -> UI ()
handlederivationclick polyStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of

      [StoredPoly name1 poly1] -> void $ element output # set UI.text ("Ergebnis: " ++ toLaTeX (derivation poly1))

      [] -> void $ element output # set UI.text "Fehler: Ableiten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."

      other -> void $ element output # set UI.text ("Fehler: Ableiten benötigt ein Polynom. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Auswertung eines Polynoms an einer bestimmten Stelle in der GUI.

Es wird das polyStore übergeben, um die gespeicherten Polynome zu lesen, das input Element, 
um den Wert für x auszulesen und das output Element, um das Ergebnis der Auswertung anzuzeigen.

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

-}

handleevaluateclick :: IORef [StoredPoly] -> Element -> Element -> UI ()
handleevaluateclick polyStore input output = do
   storedPolys <- liftIO $ readIORef polyStore
   xStr <- get value input
   case xStr of
      "" -> void $ element output # set UI.text "Fehler: Bitte geben Sie einen Wert für x ein."
      other -> case readMaybe xStr :: Maybe Rational of
         Nothing -> void $ element output # set UI.text "Fehler: Bitte geben Sie eine gültige Zahl für x ein."
         Just x -> case storedPolys of
            [StoredPoly name1 poly1] -> void $ element output # set UI.text ("Ergebnis: " ++ show (evaluate poly1 x))
            [] -> void $ element output # set UI.text "Fehler: Auswerten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."
            other -> void $ element output # set UI.text ("Fehler: Auswerten benötigt ein Polynom. Es wurde/n aber " ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung der Division von zwei Polynomen in der GUI.

Wird im Grunde genau so wie die Addition, Subtraktion und Multiplikation gehandhabt, 
nur dass wir hier die toLaTeX Funktion nicht auf ein (Poly,Poly) anwenden können und 
wir daher die Division in zwei Teile aufteilen müssen, nämlich den Quotienten und den Rest.
Somit können wir die Division von zwei Polynomen in der GUI sauber darstellen, indem wir den Quotienten und den Rest getrennt anzeigen.

-}

handledivclick :: IORef [StoredPoly] -> Element -> UI ()
handledivclick polyStore output = do
   storedPolys <- liftIO $ readIORef polyStore
   case storedPolys of 
      
      [StoredPoly name1 poly1, StoredPoly name2 poly2] -> 
         
         let (quotient, rest) = (/%) poly1 poly2 in
         void $ element output # set UI.text ("Ergebnis: " ++ name1 ++ " / " ++ name2 ++ " = " ++ toLaTeX quotient ++ ", Rest: " ++ toLaTeX rest)

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
            ("Ergebnis von " ++ name ++ ": " ++ toPrettyMathRational value)

      DivResult name quotient rest ->
         void $ element output # set UI.text
            ("Ergebnis von " ++ name ++ ": Quotient = "
             ++ toPrettyMathPoly quotient ++ ", Rest = " ++ toPrettyMathPoly rest)


