module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)
import qualified Graphics.UI.Threepenny as Ui
import qualified Control.Applicative as GUI
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Text.Read (readMaybe)

import Poly
import ParserSimple
import Tree
import Analysis
import Animation
import Format (prettyRational, toPrettyMathPoly)
import Display
import Graph
import History 
import Library
import Random
import Cache
import Parallel

{- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert z.B mit Buttons, usw... -}

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

   buttonrandompoly <- UI.button
      # set UI.html "<span class='button-symbol'>🎲</span><span>Zufallspolynom</span>"
      # set UI.class_ "operation-button"

   buttonparallel <- UI.button
      # set UI.html "<span class='button-symbol'>⚡</span><span>Parallel</span>"
      # set UI.class_ "operation-button"

   {- Action-Buttons: -}

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

   buttonsteps <- UI.button
      # set UI.html "<span class='button-symbol'>☰</span><span>Schritte</span>"
      # set UI.class_ "view-button"

   buttondetails <- UI.button
      # set UI.html "<span class='button-symbol'>🔍</span><span>Details</span>"
      # set UI.class_ "view-button"

   buttongraph <- UI.button
      # set UI.html "<span class='button-symbol'>📈</span><span>Graph</span>"
      # set UI.class_ "view-button"

   buttonhistory <- UI.button
      # set UI.html "<span class='button-symbol'>🕒</span><span>Historie</span>"
      # set UI.class_ "view-button"

   {- Ausgabebereich: -}

   {- Speicher: -}

   polyStore <- liftIO $ newIORef ([] :: PolyLibrary) --Für die Speicherung der Polynome
   resultStore <- liftIO $ newIORef NoResult --Für die Speicherung der Ergebnisse der Operationen
   historyStore <- liftIO $ newIORef (Empty :: History HistoryEntry) --Für die Speicherung der Historie der Ergebnisse (PolyResult, ValueResult, DivResult)
   cacheStore <- liftIO $ newIORef ([] :: Cache) --Für die Speicherung der Operationen, die bereits durchgeführt wurden, um sie wiederverwenden zu können, nicht die Ergebnisse einer Berechnung, sondern die Operation selbst, die durchgeführt werden soll.

   {- Ausgabebereiche: -}

   output <- UI.pre # set UI.text ""
   polyListOutput <- UI.pre # set UI.text "Noch keine Polynome vorhanden."

   inputTitle <- UI.h2 # set UI.text "Eingabe"
   polyLabel <- UI.label # set UI.text "Polynom"
   xLabel <- UI.label # set UI.text "x-Wert"
   formatHint <- UI.div
      # set UI.text "Format: Koeffizient Exponent; ..."
      # set UI.class_ "hint"

   inputPanel <- UI.div
      # set UI.class_ "panel input-panel"
      #+ [ element inputTitle
         , element polyLabel
         , element input
         , element xLabel
         , element inputX
         , element buttonaddpoly
         , element buttonrandompoly
         , element formatHint
         ]

   libraryTitle <- UI.h2 # set UI.text "Polynomliste"
   clearSelectionButton <- UI.button
      # set UI.html "<span class='small-button-symbol'>♙</span><span>Auswahl löschen</span>"
      # set UI.class_ "secondary-button"
   removePolyButton <- UI.button
      # set UI.html "<span class='small-button-symbol'>⌫</span><span>Polynom entfernen</span>"
      # set UI.class_ "secondary-button"
   libraryActions <- UI.div
      # set UI.class_ "library-actions"
      #+ [element clearSelectionButton, element removePolyButton]
   libraryPanel <- UI.div
      # set UI.class_ "panel library-panel"
      #+ [element libraryTitle, element polyListOutput, element libraryActions]

   operationsTitle <- UI.h2 # set UI.text "Polynomoperationen"
   operationGrid <- UI.div
      # set UI.class_ "operation-grid"
      #+ [ element buttonnormalize
         , element buttonnegat
         , element buttonadd
         , element buttonsub
         , element buttonmult
         , element buttondiv
         , element buttonderivation
         , element buttonevaluate
         , element buttonparallel
         ]

   displayTitle <- UI.h2 # set UI.text "Darstellung"
   displayGrid <- UI.div
      # set UI.class_ "view-grid"
      #+ [ element buttonshowresult
         , element buttonshowlatex
         , element buttontree
         , element buttonanalysis
         , element buttonsteps
         , element buttondetails
         , element buttongraph
         , element buttonhistory
         ]

   centerPanel <- UI.div
      # set UI.class_ "panel center-panel"
      #+ [ element operationsTitle
         , element operationGrid
         , UI.hr
         , element displayTitle
         , element displayGrid
         ]

   resultTitle <- UI.h2 # set UI.text "Ergebnis"
   resultOverview <- UI.div
      # set UI.class_ "result-overview"
      # set UI.html "<div><span>Operation</span><strong>-</strong></div><div><span>Eingabe</span><strong>-</strong></div><div><span>Ausgabe</span><strong>-</strong></div><div><span>Wert</span><strong>-</strong></div>"
   resultPanel <- UI.div
      # set UI.class_ "panel result-panel"
      #+ [element resultTitle, element resultOverview]

   leftColumn <- UI.div
      # set UI.class_ "left-column"
      #+ [element inputPanel, element libraryPanel]

   topGrid <- UI.div
      # set UI.class_ "top-grid"
      #+ [element leftColumn, element centerPanel, element resultPanel]

   outputTitle <- UI.h2 # set UI.text "Darstellung"
   outputTabs <- UI.div
      # set UI.class_ "output-tabs"
      #+ [ UI.span # set UI.text "Ergebnis"
         , UI.span # set UI.text "LaTeX"
         , UI.span # set UI.text "Baum"
         , UI.span # set UI.text "Analyse"
         , UI.span # set UI.text "Schritte"
         , UI.span # set UI.text "Historie"
         , UI.span # set UI.text "Graph"
         ]
   outputPanel <- UI.div
      # set UI.class_ "panel output-panel"
      #+ [element outputTitle, element outputTabs, element output]

   statusBar <- UI.div
      # set UI.class_ "status-bar"
      # set UI.html "<div><span class='status-ok'>✓</span> OK: Berechnung erfolgreich.</div><div>Haskell Kernel: aktiv <span class='status-dot'></span></div>"

   appShell <- UI.div
      # set UI.class_ "app-shell"
      #+ [element header, element topGrid, element outputPanel]

   void $ getBody window #+ [element appShell, element statusBar]

   {- ActionListener auf die Operation-Buttons: -}

   on UI.click buttonnormalize (\_ -> handlenormalizeclick input resultStore historyStore cacheStore output) 
   on UI.click buttonnegat (\_ -> handlenegatclick input resultStore historyStore cacheStore output)
   on UI.click buttonaddpoly (\_ -> handleaddpolyclick input polyListOutput polyStore)
   on UI.click buttonadd (\_ -> handleaddclick polyStore resultStore historyStore cacheStore output)
   on UI.click buttonsub (\_ -> handlesubclick polyStore resultStore historyStore cacheStore output)
   on UI.click buttonmult (\_ -> handlemultclick polyStore resultStore historyStore cacheStore output)
   on UI.click buttonderivation (\_ -> handlederivationclick polyStore resultStore historyStore cacheStore output)
   on UI.click buttonevaluate (\_ -> handleevaluateclick polyStore inputX resultStore historyStore cacheStore output)
   on UI.click buttondiv (\_ -> handledivclick polyStore resultStore historyStore cacheStore output)
   on UI.click buttonrandompoly (\_ -> handlerandompolyclick resultStore polyStore polyListOutput output)
   on UI.click buttonparallel (\_ -> handleparallelclick polyStore inputX output)

   {- ActionListener auf die Darstellung-Bbuttons: -}

   on UI.click buttonshowresult (\_ -> handleshowresultclick resultStore output)
   on UI.click buttonshowlatex (\_ -> handlelatexclick resultStore output)
   on UI.click buttontree (\_ -> handletreeclick resultStore output)
   on UI.click buttonanalysis (\_ -> handleanalysisclick resultStore output)
   on UI.click buttonsteps (\_ -> handlestepsclick resultStore output)
   on UI.click buttondetails (\_ -> handledetailsclick resultStore output)
   on UI.click buttongraph (\_ -> handlegraphclick resultStore output)
   on UI.click buttonhistory (\_ -> handlehistoryclick historyStore output)   

{- Operationshandler -}

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

handlenormalizeclick :: Element -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handlenormalizeclick input resultStore historyStore cacheStore output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->
         void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Normalize poly
         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)
            Nothing -> do
               let resultPoly = normalize poly
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               liftIO $ writeIORef resultStore (PolyResult "Normalisieren" resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly "Normalisieren" resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)
               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)
{- 

Diese Funktion dient zur Veranschaulichung eines negierten Polynoms in der GUI.

Gleiche Logik wie bei handlenormalizeclick, nur dass hier die Funktion negat aufgerufen wird, um das Polynom zu negieren.

Auch hier wird das Ergebnis zusätzlich in resultStore gespeichert.
Das ist wichtig, damit man danach z.B. auf den LaTeX-Button klicken kann, ohne dass nochmal neu gerechnet werden muss.

Da negat wieder ein Polynom zurückgibt, speichern wir das Ergebnis als PolyResult.

-}

handlenegatclick :: Element -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handlenegatclick input resultStore historyStore cacheStore output = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->
         void $ element output # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Negate poly
         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)
            Nothing -> do
               let resultPoly = negat poly
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               liftIO $ writeIORef resultStore (PolyResult "Negieren" resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly "Negieren" resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)
               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

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

handleaddpolyclick :: Element -> Element -> IORef PolyLibrary -> UI ()
handleaddpolyclick input polyListOutput polyStore = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->
         void $ element polyListOutput # set UI.text ("Fehler: " ++ err)
      Right poly -> do
         library <- liftIO $ readIORef polyStore
         let name = "p" ++ show (length library + 1)
         let newLibrary = savePoly name poly library
         liftIO $ writeIORef polyStore newLibrary
         void $ element polyListOutput # set UI.text (showPolyLibraryText newLibrary)

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

handleaddclick :: IORef PolyLibrary -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handleaddclick polyStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   case library of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Add poly1 poly2

         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)

            Nothing -> do
               let resultPoly = add poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               liftIO $ writeIORef resultStore (PolyResult (name1 ++ " + " ++ name2) resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " + " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)

               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

      [] ->
         void $ element output # set UI.text "Fehler: Addieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other ->
         void $ element output # set UI.text
            ("Fehler: Addieren benötigt zwei Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung der Subtraktion von zwei Polynomen in der GUI.

Funktioniert genau wie handleaddclick, nur dass hier die Funktion sub aufgerufen wird, um die Subtraktion durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da sub wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handlesubclick :: IORef PolyLibrary -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handlesubclick polyStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   case library of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Sub poly1 poly2

         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)

            Nothing -> do
               let resultPoly = sub poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               liftIO $ writeIORef resultStore (PolyResult (name1 ++ " - " ++ name2) resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " - " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)

               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

      [] ->
         void $ element output # set UI.text "Fehler: Subtrahieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other ->
         void $ element output # set UI.text
            ("Fehler: Subtrahieren benötigt zwei Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e gespeichert.")


{- 

Diese Funktion dient zur Veranschaulichung der Multiplikation von zwei Polynomen in der GUI.

Funktioniert genau wie handleaddclick, nur dass hier die Funktion mult aufgerufen wird, um die Multiplikation durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da mult wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handlemultclick :: IORef PolyLibrary -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handlemultclick polyStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   case library of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Mul poly1 poly2

         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)

            Nothing -> do
               let resultPoly = mult poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               liftIO $ writeIORef resultStore (PolyResult (name1 ++ " * " ++ name2) resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " * " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)

               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

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

handlederivationclick :: IORef PolyLibrary -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handlederivationclick polyStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   case library of
      [(name1, poly1)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Derive poly1
         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)
            Nothing -> do
               let resultPoly = derivation poly1
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               liftIO $ writeIORef resultStore (PolyResult (name1 ++ "'") resultPoly)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly "Ableiten" resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)
               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)
      [] ->
         void $ element output # set UI.text "Fehler: Ableiten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."
      other ->
         void $ element output # set UI.text
            ("Fehler: Ableiten benötigt ein Polynom. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e gespeichert.")


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

handleevaluateclick :: IORef PolyLibrary -> Element -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handleevaluateclick polyStore input resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   xStr <- get value input
   case xStr of
      "" ->
         void $ element output # set UI.text "Fehler: Bitte geben Sie einen Wert für x ein."

      _ -> case readMaybe xStr :: Maybe Rational of
         Nothing ->
            void $ element output # set UI.text "Fehler: Bitte geben Sie eine gültige Zahl für x ein."

         Just x -> case library of
            [(name1, poly1)] -> do
               cache <- liftIO $ readIORef cacheStore
               let operation = Evaluate poly1 x

               case lookupCache operation cache of
                  Just cachedResult ->
                     void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)

                  Nothing -> do
                     let resultValue = evaluate poly1 x
                     let resultText = "Ergebnis: " ++ prettyRational resultValue

                     liftIO $ writeIORef resultStore (ValueResult (name1 ++ "(" ++ show x ++ ")") resultValue)
                     liftIO $ modifyIORef historyStore
                        (addHistory (HistoryValue (name1 ++ "(" ++ show x ++ ")") resultValue))
                     liftIO $ modifyIORef cacheStore
                        (insertCache operation resultText)

                     void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

            [] ->
               void $ element output # set UI.text "Fehler: Auswerten benötigt ein Polynom. Es wurde noch kein Polynom gespeichert."

            other ->
               void $ element output # set UI.text
                  ("Fehler: Auswerten benötigt ein Polynom. Es wurde/n aber "
                   ++ show (length other) ++ " Polynom/e gespeichert.")

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

handledivclick :: IORef PolyLibrary -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef Cache -> Element -> UI ()
handledivclick polyStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   case library of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Div poly1 poly2

         case lookupCache operation cache of
            Just cachedResult ->
               void $ element output # set UI.text ("Aus Cache geladen:\n" ++ cachedResult)

            Nothing -> do
               let (quotient, rest) = (/%) poly1 poly2
               let resultText =
                     "Ergebnis: " ++ name1 ++ " / " ++ name2
                     ++ " = " ++ toPrettyMathPoly quotient
                     ++ ", Rest: " ++ toPrettyMathPoly rest

               liftIO $ writeIORef resultStore (DivResult (name1 ++ " / " ++ name2) quotient rest)
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryDiv (name1 ++ " / " ++ name2) quotient rest))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation resultText)

               void $ element output # set UI.text ("Neu berechnet:\n" ++ resultText)

      [] ->
         void $ element output # set UI.text "Fehler: Dividieren benötigt zwei Polynome. Es wurde noch kein Polynom gespeichert."

      other ->
         void $ element output # set UI.text
            ("Fehler: Dividieren benötigt zwei Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e gespeichert.")

{- 

Diese Funktion dient zur Veranschaulichung eines zufälligen Polynoms in der GUI.
Sie wird aufgerufen, wenn der Button "Zufallspolynom" geklickt wird.

Sie erhält resultStore, polyStore, polyListOutput und output übergeben.
resultStore ist der Speicher für das Ergebnis der Operation, 
polyStore ist der Speicher für die gespeicherten Polynome,
polyListOutput ist der Ausgabebereich für die Polynomliste und output ist der Ausgabebereich für das Ergebnis.

library ist ist die aktuelle PolynomBibliothek, die wir aus polyStore auslesen.
poly wird mit randomPoly erzeugt, welches ein zufälliges Polynom generiert.

Danach wird ein Name für das Polynom erzeugt, z.B. r1, r2, r3 usw.
newLibrary ist die neue PolynomBibliothek, die das neue zufällige Polynom enthält.

Wir speichern die neue PolynomBibliothek in polyStore und das Ergebnis in resultStore.

Zuletzt zeigen wir die neue Polynomliste in polyListOutput an und das Ergebnis in output.

-}

handlerandompolyclick :: IORef GuiResult -> IORef PolyLibrary -> Element -> Element -> UI ()
handlerandompolyclick resultStore polyStore polyListOutput output = do
   library <- liftIO $ readIORef polyStore
   poly <- liftIO randomPoly
   let name = "r" ++ show (length library + 1)
   let newLibrary = savePoly name poly library
   liftIO $ writeIORef polyStore newLibrary
   liftIO $ writeIORef resultStore (PolyResult name poly)
   void $ element polyListOutput # set UI.text (showPolyLibraryText newLibrary)
   void $ element output # set UI.text (showRandomPolyText name poly)
   
{- 

Diese Funktion dient wird aufgerufen, wenn der Button "Parallel" geklickt wird.
Sie dient dazu, die Auswertung von mehreren Polynomen an einer bestimmten Stelle parallel durchzuführen.

Sie nekommt polyStore, input und output übergeben. polyStore ist der Speicher für die gespeicherten Polynome, 
input ist das Eingabefeld für den Wert von x und output ist der Ausgabebereich für das Ergebnis.

Danach wird library erstellt, das die aktuelle PolynomBibliothek aus polyStore ausliest und xStr wird erstellt, 
das den Wert von x aus dem Eingabefeld input ausliest.

xStr wird auf zwei Fälle geprüft:
Fall 1: xStr ist leer, dann wird eine Fehlermeldung angezeigt.
Fall 2: xStr ist nicht leer, dann wird geprüft, ob es sich vielleicht um eine gültige 
Zahl (Just x) handelt oder auch nicht (Nothing), mithilfe von readMaybe:

Fall 2.1: Es handelt sich nicht um eine gültige Zahl, dann wird eine Fehlermeldung angezeigt.
Fall 2.2: Es handelt sich um eine gültige Zahl, dann prüfen wir die gespeicherten Polynome auf 2 Fälle:

Fall 2.2.1: Es ist kein Polynom gespeichert, dann wird eine Fehlermeldung angezeigt.
Fall 2.2.2 (Andernfalls): Es sind ein oder mehrere Polynome gespeichert, dann wird die Auswertung parallel durchgeführt und das Ergebnis angezeigt.

-}

handleparallelclick :: IORef PolyLibrary -> Element -> Element -> UI ()
handleparallelclick polyStore input output = do
   library <- liftIO $ readIORef polyStore
   xStr <- get value input
   case xStr of
      ""->
         void $ element output # set UI.text "Fehler: Bitte geben Sie einen Wert für x ein."
      _ -> case readMaybe xStr :: Maybe Rational of
         Nothing ->
            void $ element output # set UI.text "Fehler: Bitte geben Sie eine gültige Zahl für x ein."
         Just x -> do
            case library of
               [] -> void $ element output # set UI.text "Fehler: Es wurde noch kein Polynom gespeichert."
               _ -> do
                  let results = evaluateNamedManyParallel x library
                  void $ element output # set UI.text (showParallelResultsText x results)

{- Darstellungshandler -}

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
      PolyResult name poly -> void $ element output # set UI.text (showResultText name poly)
      ValueResult name value -> void $ element output # set UI.text (showValueText name value)
      DivResult name quotient rest ->
         void $ element output # set UI.text
            (showDivResultText name quotient rest)

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
         void $ element output # set UI.text (showTreeText name poly)
      ValueResult name value -> do
         void $ element output # set UI.text (showValueTreeText name value)
      DivResult name quotient rest -> do
         void $ element output # set UI.text (showDivTreeText name quotient rest)

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
         void $ element output # set UI.text (showAnalysisText name poly)
      ValueResult name value -> do
         void $ element output # set UI.text (showValueAnalysisText name value)
      DivResult name quotient rest -> do
         void $ element output # set UI.text (showDivAnalysisText name quotient rest)


{- 

Diese Funktion startet die Animation der Traversierung eines Baumes in der GUI.

Sie bekommt den Baum, die Traversierungsschritte und den Ausgabebereich output übergeben.
Es wird ein IORef stepStore erstellt, um den aktuellen Schritt der Traversierung zu speichern.

Wir erstellen einen Timer, der alle 700 Millisekunden tickt.

Mit on UI.tick timer $ \_ -> do sagen wir, was passieren soll, wenn der Timer (also jedes mal, wenn er tickt) tickt.
Innerhalb des Timers wird der aktuelle Schritt aus stepStore gelesen.
Wenn der aktuelle Schritt größer oder gleich der Länge der Traversierungsschritte ist, wird der Timer gestoppt und eine Nachricht angezeigt, dass die Traversierung abgeschlossen ist.
Ansonsten wird mit currentStep der aktuelle Traversierungsschritt aus der Liste der Traversierungsschritte geholt und im Ausgabebereich angezeigt.
Der Baum wird dabei ebenfalls im schönen Format angezeigt, damit der Benutzer den aktuellen Zustand des Baumes sehen kann.
zuletzt wird der aktuelle Schritt um 1 erhöht, damit beim nächsten Tick der nächste Schritt angezeigt wird.

-}

startTraversalAnimation :: ExprTree -> [TraversalStep] -> Element -> UI ()
startTraversalAnimation tree steps output = do
   stepStore <- liftIO $ newIORef 0

   timer <- UI.timer # set UI.interval 700

   on UI.tick timer $ \_ -> do
      currentIndex <- liftIO $ readIORef stepStore
      if currentIndex >= length steps
         then do
            UI.stop timer
            void $ element output # set UI.text (showStepsText tree steps currentIndex)
         else do
            void $ element output # set UI.text (showStepsText tree steps currentIndex)
            liftIO $ writeIORef stepStore (currentIndex + 1)

   UI.start timer

{- 

Diese Funktion wird aufgerufen, wenn der Button "Schritte" geklickt wird.
Sie dient dazu, die Traversierung eines Baumes anzuzeigen.

Wir übergeben resultStore, um das Ergebnis der Berechnung zu lesen, und output, um die Traversierung anzuzeigen.
Danach wird geprüft, ob es ein Ergebnis gibt oder nicht, indem das Ergebnis auf vier Fälle überprüft wird.

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann wird das Polynom in einen Baum umgewandelt und die Traversierung angezeigt.
Fall 3: Das Ergebnis ist ein Wert, dann wird der Wert in einen Baum umgewandelt und die Traversierung angezeigt.
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann wird der Quotient und der Rest in einen Baum umgewandelt und die Traversierung angezeigt.

-}

handlestepsclick :: IORef GuiResult -> Element -> UI ()
handlestepsclick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult name poly -> do
         let tree = polyToExprTree poly
         let traversal = preOrder tree
         let steps = makeTraversalSteps traversal
         startTraversalAnimation tree steps output
      ValueResult name value -> do
         let tree = TConst value
         let traversal = preOrder tree
         let steps = makeTraversalSteps traversal
         startTraversalAnimation tree steps output
      DivResult name quotient rest -> do
         let tree = polyToExprTree quotient
         let traversal = preOrder tree
         let steps = makeTraversalSteps traversal
         startTraversalAnimation tree steps output

{-

Diese Funktion wird aufgerufen, wenn der Button "Details" geklickt wird.
Sie dient dazu, die Details eines Ergebnisses anzuzeigen.

Wir übergeben resultStore, um das Ergebnis der Berechnung zu lesen, und output, um die Details anzuzeigen.
Danach wird geprüft, ob es ein Ergebnis gibt oder nicht, indem das Ergebnis auf vier Fälle überprüft wird.

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann werden die Details des Polynoms angezeigt.
Fall 3: Das Ergebnis ist ein Wert, dann werden die Details des Wertes angezeigt.
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann werden die Details des

-}

handledetailsclick :: IORef GuiResult -> Element -> UI ()
handledetailsclick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult name poly ->
         void $ element output # set UI.text (showDetailsText name poly)
      ValueResult name value ->
         void $ element output # set UI.text (showValueDetailsText name value)
      DivResult name quotient rest ->
         void $ element output # set UI.text (showDivDetailsText name quotient rest)

{- 

Diese Funktion wird aufgerufen, wenn der Button "Graph" geklickt wird.
Sie dient dazu, den Graphen eines Polynoms anzuzeigen.

Funktioniert ähnlich wie die anderen Darstellungsfunktionen, nur dass hier die Funktion graphView 
aufgerufen wird, um den Graphen des Polynoms zu erstellen.

Ebenfalls nutzen wir bei einer gültigen Ausgabe eines FunktionsGraphen UI.html anstatt UI.text, 
da wir hier HTML-Code zurückgeben, um den Graphen in der GUI anzuzeigen.

-}

handlegraphclick :: IORef GuiResult -> Element -> UI ()
handlegraphclick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         void $ element output # set UI.text "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult name poly -> do
         let graph = graphView poly
         void $ element output # set UI.html (showGraphText name poly)
      ValueResult name value -> do
         void $ element output # set UI.html (showValueGraphText name value)
      DivResult name quotient rest -> do
         void $ element output # set UI.html (showGraphDivText name quotient rest)

{- 

Diese Funktion wird aufgerufen, wenn der Button "Historie" geklickt wird.
Sie dient dazu, die History der Ergebnisse von Berechnungen anzuzeigen.

Sie bekommt die History aus historyStore und den Ausgabebereich output übergeben.
Es wird "history" erstellt, um die gespeicherte History aus historyStore zu lesen und auf 2 Fälle zu prüfen:

1. Fall: Die History ist leer, dann wird eine Fehlermeldung angezeigt.
2. Fall: Die History enthält Einträge, dann werden diese mithilfe showHistoryText angezeigt.

-}

handlehistoryclick :: IORef (History HistoryEntry) -> Element -> UI ()
handlehistoryclick historyStore output = do
   history <- liftIO $ readIORef historyStore
   case history of
      Empty -> void $ element output # set UI.text "Fehler: Es wurde noch keine Historie erstellt."
      other -> void $ element output # set UI.text (showHistoryText history)
