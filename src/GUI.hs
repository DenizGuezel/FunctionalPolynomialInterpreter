module GUI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (intercalate)

import Poly
import ParserSimple
import Tree
import Analysis
import Animation
import Format (prettyRational, toPrettyMathPoly)
import Display
import History 
import Library
   ( PolyLibrary
   , savePoly
   , selectedPolys
   , nextPolyName
   , nextRandomName
   )
import Random
import Cache
import Parallel
import Examples

{- Hier kommt die GUI-Logik rein, welche die Interaktion mit dem Benutzer steuert, z.B. mit Buttons. Die GUI benutzt die anderen Module, um die Interaktion zu ermöglichen. -}

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
   | PolyResult String String Poly
   | ValueResult String String Rational
   | DivResult String String Poly Poly
   | ParallelResult Rational [(String, Rational)]
   deriving (Show, Eq)

{-

Dieser Datentyp beschreibt, ob eine GUI-Aktion erfolgreich war oder fehlgeschlagen ist.

GuiSuccess enthält eine kurze Erfolgsmeldung für die Statusleiste.
GuiError enthält die konkrete Fehlermeldung für die Statusleiste.

Dadurch müssen Fehlermeldungen nicht gleichzeitig im Darstellungsbereich und in der Statusleiste stehen.
Der Darstellungsbereich bleibt bei Fehlern neutral und die Statusleiste zeigt den eigentlichen Fehler.

-}

data GuiActionResult
   = GuiSuccess String
   | GuiError String
   deriving (Show, Eq)

{- 

Diese Funktion cachedToGuiResult wird verwendet, um ein CachedResult in ein GuiResult umzuwandeln.
Sie nimmt als Eingabe einen Namen (String) und ein CachedResult und gibt ein GuiResult zurück.

Je nachdem, ob das CachedResult ein Polynom, ein Wert oder eine Division ist, wird das entsprechende GuiResult erstellt.

-}

cachedToGuiResult :: String -> String -> CachedResult -> GuiResult
cachedToGuiResult operation inputText (CachedPoly poly) = PolyResult operation inputText poly
cachedToGuiResult operation inputText (CachedValue resultValue) = ValueResult operation inputText resultValue
cachedToGuiResult operation inputText (CachedDiv quotient rest) = DivResult operation inputText quotient rest

{- Diese Funktion gibt den Text zurück, der angezeigt wird, wenn kein gültiges Ergebnis vorhanden ist. -}

noValidResultText :: String
noValidResultText = "Keine gültige Berechnung vorhanden."

{- 

Diese Funktion resultOverviewHtml wird verwendet, um das Ergebnis einer GUI-Operation in HTML darzustellen.
Sie nimmt ein GuiResult als Eingabe und gibt einen String zurück, der HTML-Code enthält, um das Ergebnis in der GUI anzuzeigen.

Diese Funktion bleibt in Gui.hs, da sie nicht nur allgemeine Textanzeige tätigt, 
sondern spezifisch für die GUI ist, da sie HTML-Code erzeugt, der in der GUI angezeigt wird.

-}

resultOverviewHtml :: GuiResult -> String
resultOverviewHtml NoResult =
   "<div><span>Operation</span><strong>-</strong></div>"
   ++ "<div><span>Eingabe</span><strong>-</strong></div>"
   ++ "<div><span>Ausgabe</span><strong>-</strong></div>"
   ++ "<div><span>Wert</span><strong>-</strong></div>"
resultOverviewHtml (PolyResult operation inputText poly) =
   "<div><span>Operation</span><strong>" ++ operation ++ "</strong></div>"
   ++ "<div><span>Eingabe</span><strong>" ++ inputText ++ "</strong></div>"
   ++ "<div><span>Ausgabe</span><strong>" ++ toPrettyMathPoly poly ++ "</strong></div>"
   ++ "<div><span>Wert</span><strong>-</strong></div>"
resultOverviewHtml (ValueResult operation inputText resultValue) =
   "<div><span>Operation</span><strong>" ++ operation ++ "</strong></div>"
   ++ "<div><span>Eingabe</span><strong>" ++ inputText ++ "</strong></div>"
   ++ "<div><span>Ausgabe</span><strong>1 Wert</strong></div>"
   ++ "<div><span>Wert</span><strong>" ++ prettyRational resultValue ++ "</strong></div>"
resultOverviewHtml (DivResult operation inputText quotient rest) =
   "<div><span>Operation</span><strong>" ++ operation ++ "</strong></div>"
   ++ "<div><span>Eingabe</span><strong>" ++ inputText ++ "</strong></div>"
   ++ "<div><span>Ausgabe</span><strong>Q = " ++ toPrettyMathPoly quotient ++ "</strong></div>"
   ++ "<div><span>Wert</span><strong>R = " ++ toPrettyMathPoly rest ++ "</strong></div>"
resultOverviewHtml (ParallelResult x results) =
   "<div><span>Operation</span><strong>Parallel</strong></div>"
   ++ "<div><span>Eingabe</span><strong>x = " ++ prettyRational x ++ "</strong></div>"
   ++ "<div><span>Ausgabe</span><strong>" ++ resultCountText results ++ "</strong></div>"
   ++ "<div><span>Wert</span><strong>" ++ parallelResultOverviewText results ++ "</strong></div>"

{- Diese Hilfsfunktion stellt die Anzahl der parallelen Werte im Ergebnisfeld dar. -}

resultCountText :: [(String, Rational)] -> String
resultCountText [_] = "1 Wert"
resultCountText results = show (length results) ++ " Werte"

{- Diese Hilfsfunktion stellt die parallelen Ergebnisse kurz im Ergebnisfeld dar. -}

parallelResultOverviewText :: [(String, Rational)] -> String
parallelResultOverviewText results =
   intercalate ", " [name ++ ": " ++ prettyRational resultValue | (name, resultValue) <- results]

{- 

Diese Funktion wandelt ein GuiResult in einen normalen Ausgabetext um.
Sie wird besonders für den Cache benutzt.

Wenn ein Ergebnis aus dem Cache geladen wird, soll nicht nur ein alter Text angezeigt werden,
sondern das echte GuiResult wird wieder in resultStore geschrieben.
Danach kann diese Funktion das GuiResult trotzdem gut lesbar im Ausgabebereich anzeigen.

Je nachdem , ob das GuiResult ein Polynom, ein Wert oder eine Division ist, wird die entsprechende Text-Funktion aus Display.hs aufgerufen.

-}

guiResultText :: GuiResult -> String
guiResultText NoResult = "Fehler: Es wurde noch kein Ergebnis berechnet."
guiResultText (PolyResult _ name poly) = showResultText name poly
guiResultText (ValueResult _ name resultValue) = showValueText name resultValue
guiResultText (DivResult _ name quotient rest) = showDivResultText name quotient rest
guiResultText (ParallelResult x results) = showParallelResultsText x results

{-

Diese Hilfsfunktionen schreiben eine Ausgabe in die GUI und geben gleichzeitig ein GuiActionResult zurück.

setOutputSuccess wird benutzt, wenn eine Aktion erfolgreich war und normalen Text anzeigt.
setOutputHtmlSuccess wird benutzt, wenn eine Aktion erfolgreich war und HTML anzeigt, z.B. beim Graphen.
setOutputError wird benutzt, wenn eine Aktion fehlgeschlagen ist.

Dadurch entscheidet nicht mehr der Text im Ausgabefeld, ob etwas erfolgreich war.
Der Handler gibt den Zustand direkt typisiert zurück.

-}

setOutputSuccess :: Element -> String -> UI GuiActionResult
setOutputSuccess output outputText = do
   void $ element output # set UI.text outputText
   return (GuiSuccess "OK: Berechnung erfolgreich.")

setOutputHtmlSuccess :: Element -> String -> UI GuiActionResult
setOutputHtmlSuccess output htmlText = do
   void $ element output # set UI.html htmlText
   return (GuiSuccess "OK: Berechnung erfolgreich.")

setOutputError :: Element -> String -> UI GuiActionResult
setOutputError output message = do
   void $ element output # set UI.text message
   return (GuiError message)

{-

Diese Hilfsfunktionen machen dasselbe für Meldungen der Polynomliste.

Die Polynomliste hat einen eigenen kleinen Meldungsbereich.
Darum schreiben wir hier nicht in den großen Darstellungsbereich, geben aber trotzdem ein GuiActionResult zurück.

-}

setPolyListSuccess :: Element -> String -> UI GuiActionResult
setPolyListSuccess polyListMessage message = do
   void $ element polyListMessage # set UI.text message
   return (GuiSuccess message)

clearPolyListSuccess :: Element -> String -> UI GuiActionResult
clearPolyListSuccess polyListMessage statusMessage = do
   void $ element polyListMessage # set UI.text ""
   return (GuiSuccess statusMessage)

setPolyListError :: Element -> String -> UI GuiActionResult
setPolyListError polyListMessage message = do
   void $ element polyListMessage # set UI.text message
   return (GuiError message)

{- Diese Funktion startet Threepenny mit Standardkonfiguration und benutzt dabei setup um das Fenster aufzubauen. -}

runGUI :: IO () 
runGUI = startGUI defaultConfig { jsStatic = Just "static" } setup

{-

Diese Funktion wird von Threepenny aufgerufen, um das Fenster aufzubauen. 
Sie bekommt ein Window übergeben, in dem sie die GUI-Elemente platzieren kann.

Threepenny nutzt im Hintergrund HTML und CSS, z.B. um Buttons, Textfelder, etc. darzustellen.
z.B. ist UI.button ein Button, ähnlich wie <button> </button> in HTML.

Mit void $ return window # set title "Polynom-Parser" setzen wir den Fenstertitel.

Mit headline definieren wir eine Überschrift, die wir mit UI.h1 erstellen, welche als Überschrift dient.
Mit input definieren wir ein Eingabefeld, in das der Benutzer ein Polynom eingeben kann.
Mit button definieren wir einen Button, der zum Parsen des Polynoms verwendet werden kann.
Mit output definieren wir einen Bereich (div), in dem das Ergebnis des Parsens angezeigt werden kann.

wir definieren jeweils vor dem <- den Namen des Elements, nach dem <- sagen wir erst, um welches Element es sich handelt (z.B. UI.h1 ist in HTML <h1> </h1>),
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
      # set UI.html "&lambda;"
      # set UI.class_ "lambda-logo"

   headline <- UI.h1
      # set UI.text "Functional Polynomial Interpreter"
      # set UI.class_ "app-title"

   subtitle <- UI.div
      # set UI.text "Ein symbolischer Polynomrechner in Haskell"
      # set UI.class_ "app-subtitle"

   titleBlock <- UI.div
      # set UI.class_ "title-block"
      #+ [element headline, element subtitle]

   header <- UI.div
      # set UI.class_ "header"
      #+ [element lambdaLogo, element titleBlock]

   {- Eingabefelder -}

   input <- UI.input # set (attr "placeholder") "Polynom hinzufügen"
   inputX <- UI.input # set (attr "placeholder") "x-Wert"

   {- Operation-Buttons -}

   buttonNormalize <- UI.button
      # set UI.html "<span class='button-symbol'>N</span><span>Normalisieren</span>"
      # set UI.class_ "operation-button"

   buttonNegate <- UI.button
      # set UI.html "<span class='button-symbol'>&minus;p</span><span>Negieren</span>"
      # set UI.class_ "operation-button"

   buttonAddPoly <- UI.button
      # set UI.html "<span class='button-symbol'>+</span><span>Polynom hinzuf&uuml;gen</span>"
      # set UI.class_ "operation-button"

   buttonAdd <- UI.button
      # set UI.html "<span class='button-symbol'>+</span><span>Addieren</span>"
      # set UI.class_ "operation-button"

   buttonSub <- UI.button
      # set UI.html "<span class='button-symbol'>&minus;</span><span>Subtrahieren</span>"
      # set UI.class_ "operation-button"

   buttonMult <- UI.button
      # set UI.html "<span class='button-symbol'>&times;</span><span>Multiplizieren</span>"
      # set UI.class_ "operation-button"

   buttonDerivation <- UI.button
      # set UI.html "<span class='button-symbol'>d/dx</span><span>Ableiten</span>"
      # set UI.class_ "operation-button"

   buttonEvaluate <- UI.button
      # set UI.html "<span class='button-symbol'>f(x)</span><span>Auswerten</span>"
      # set UI.class_ "operation-button"

   buttonDiv <- UI.button
      # set UI.html "<span class='button-symbol'>&divide;</span><span>Dividieren</span>"
      # set UI.class_ "operation-button"

   buttonRandomPoly <- UI.button
      # set UI.html "<span class='button-symbol'>&#127922;</span><span>Zufallspolynom</span>"
      # set UI.class_ "operation-button"

   buttonParallel <- UI.button
      # set UI.html "<span class='button-symbol'>&#9889;</span><span>Parallel</span>"
      # set UI.class_ "operation-button"

   buttonLoadExamples <- UI.button
      # set UI.html "<span class='button-symbol'>Ex</span><span>Beispiele laden</span>"
      # set UI.class_ "operation-button"

   {- Darstellung-Buttons -}

   buttonShowResult <- UI.button
      # set UI.html "<span class='button-symbol'>i</span><span>Ergebnis</span>"
      # set UI.class_ "view-button"

   buttonShowLatex <- UI.button
      # set UI.html "<span class='button-symbol'>TeX</span><span>LaTeX</span>"
      # set UI.class_ "view-button"

   buttonTree <- UI.button
      # set UI.html "<span class='button-symbol'>&#127795;</span><span>Baum</span>"
      # set UI.class_ "view-button"

   buttonAnalysis <- UI.button
      # set UI.html "<span class='button-symbol'>&#128202;</span><span>Analyse</span>"
      # set UI.class_ "view-button"

   buttonSteps <- UI.button
      # set UI.html "<span class='button-symbol'>&#9776;</span><span>Schritte</span>"
      # set UI.class_ "view-button"

   buttonDetails <- UI.button
      # set UI.html "<span class='button-symbol'>&#128269;</span><span>Details</span>"
      # set UI.class_ "view-button"

   buttonGraph <- UI.button
      # set UI.html "<span class='button-symbol'>&#128200;</span><span>Graph</span>"
      # set UI.class_ "view-button"

   buttonHistory <- UI.button
      # set UI.html "<span class='button-symbol'>&#128338;</span><span>Historie</span>"
      # set UI.class_ "view-button"

   {- Speicher -}

   polyStore <- liftIO $ newIORef ([] :: PolyLibrary) --Für die Speicherung der Polynome
   resultStore <- liftIO $ newIORef NoResult --Für die Speicherung der Ergebnisse der Operationen
   historyStore <- liftIO $ newIORef (Empty :: History HistoryEntry) --Für die Speicherung der Historie der Ergebnisse (PolyResult, ValueResult, DivResult)
   cacheStore <- liftIO $ newIORef ([] :: Cache CachedResult) --Für die Speicherung der Operationen, die bereits durchgeführt wurden, um sie wiederverwenden zu können, nicht die Ergebnisse einer Berechnung, sondern die Operation selbst, die durchgeführt werden soll.
   selectedStore <- liftIO $ newIORef ([] :: [String]) --Für die Speicherung der ausgewählten Polynome in der GUI, um sie für Operationen wie Addieren, Subtrahieren, Multiplizieren, Dividieren, etc. zu verwenden. Sinnvoll für eine Checkbox-Liste, damit wir bestimmte Polynome auswählen können.
   traversalRunningStore <- liftIO $ newIORef False --Merkt sich, ob gerade eine Traversierungsanimation laeuft, damit nicht mehrere Timer gleichzeitig gestartet werden.
   
   {- Ausgabebereiche -}

   output <- UI.pre # set UI.text ""
   polyListOutput <- UI.div # set UI.class_ "poly-list"
   polyListMessage <- UI.div
      # set UI.class_ "hint"
      # set UI.text ""

   {- Eingabebereiche -}

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
         , element buttonAddPoly
         , element buttonRandomPoly
         , element buttonLoadExamples
         , element formatHint
         ]

   {- Polynomliste -}

   libraryTitle <- UI.h2 # set UI.text "Polynomliste"
   clearSelectionButton <- UI.button
      # set UI.html "<span class='small-button-symbol'>&#9817;</span><span>Auswahl l&ouml;schen</span>"
      # set UI.class_ "secondary-button"
   removePolyButton <- UI.button
      # set UI.html "<span class='small-button-symbol'>&#9003;</span><span>Polynom entfernen</span>"
      # set UI.class_ "secondary-button"
   libraryActions <- UI.div
      # set UI.class_ "library-actions"
      #+ [element clearSelectionButton, element removePolyButton]
   libraryPanel <- UI.div
      # set UI.class_ "panel library-panel"
      #+ [element libraryTitle, element polyListOutput, element polyListMessage, element libraryActions]

   {- Speicherung der Operation-Buttons -}

   operationsTitle <- UI.h2 # set UI.text "Polynomoperationen"
   operationGrid <- UI.div
      # set UI.class_ "operation-grid"
      #+ [ element buttonNormalize
         , element buttonNegate
         , element buttonAdd
         , element buttonSub
         , element buttonMult
         , element buttonDiv
         , element buttonDerivation
         , element buttonEvaluate
         , element buttonParallel
         ]

   {- Speicherung der Darstellung-Buttons-}

   displayTitle <- UI.h2 # set UI.text "Darstellungsoperationen"
   displayGrid <- UI.div
      # set UI.class_ "view-grid"
      #+ [ element buttonShowResult
         , element buttonShowLatex
         , element buttonTree
         , element buttonAnalysis
         , element buttonSteps
         , element buttonDetails
         , element buttonGraph
         , element buttonHistory
         ]

   {- Zentraler Steuerungsbereich-}

   centerPanel <- UI.div
      # set UI.class_ "panel center-panel"
      #+ [ element operationsTitle
         , element operationGrid
         , UI.hr
         , element displayTitle
         , element displayGrid
         ]

   {- Ergebnisübersicht-}

   resultTitle <- UI.h2 # set UI.text "Ergebnis"
   resultOverview <- UI.div
      # set UI.class_ "result-overview"
      # set UI.html (resultOverviewHtml NoResult)
   resultPanel <- UI.div
      # set UI.class_ "panel result-panel"
      #+ [element resultTitle, element resultOverview]

   leftColumn <- UI.div
      # set UI.class_ "left-column"
      #+ [element inputPanel, element libraryPanel]

   {- Tabs für die Darstellung der Ergebnisse: -}

   outputTitle <- UI.h2 # set UI.text "Darstellung"
   outputTabResult <- UI.span # set UI.text "Ergebnis" # set UI.class_ "active"
   outputTabLatex <- UI.span # set UI.text "LaTeX"
   outputTabTree <- UI.span # set UI.text "Baum"
   outputTabAnalysis <- UI.span # set UI.text "Analyse"
   outputTabSteps <- UI.span # set UI.text "Schritte"
   outputTabDetails <- UI.span # set UI.text "Details"
   outputTabHistory <- UI.span # set UI.text "Historie"
   outputTabGraph <- UI.span # set UI.text "Graph"

   {- Speicherung der Output-Tabs -}

   outputTabs <- UI.div
      # set UI.class_ "output-tabs"
      #+ [ element outputTabResult
         , element outputTabLatex
         , element outputTabTree
         , element outputTabAnalysis
         , element outputTabSteps
         , element outputTabDetails
         , element outputTabHistory
         , element outputTabGraph
         ]

   {- Darstellung des Output-Bereichs -}

   outputPanel <- UI.div
      # set UI.class_ "panel output-panel"
      #+ [element outputTitle, element outputTabs, element output]

   mainTop <- UI.div
      # set UI.class_ "main-top"
      #+ [element centerPanel, element resultPanel]

   mainColumn <- UI.div
      # set UI.class_ "main-column"
      #+ [element mainTop, element outputPanel]

   topGrid <- UI.div
      # set UI.class_ "top-grid"
      #+ [element leftColumn, element mainColumn]

   _statusBar <- UI.div
      # set UI.class_ "status-bar"
      # set UI.html "<div><span class='status-ok'>&#10003;</span> OK: Berechnung erfolgreich.</div><div>Haskell Kernel: aktiv <span class='status-dot'></span></div>"

   {- Dynamische Statusleiste -}

   statusIcon <- UI.span
      # set UI.class_ "status-ok"
      # set UI.html "&#10003;"

   statusText <- UI.span
      # set UI.text "Bereit."

   statusLeft <- UI.div
      # set UI.class_ "status-left"
      #+ [element statusIcon, element statusText]

   statusRight <- UI.div
      # set UI.html "Haskell Kernel: aktiv <span class='status-dot'></span>"

   dynamicStatusBar <- UI.div
      # set UI.class_ "status-bar"
      #+ [element statusLeft, element statusRight]

   appShell <- UI.div
      # set UI.class_ "app-shell"
      #+ [element header, element topGrid]

   void $ getBody window #+ [element appShell, element dynamicStatusBar]

   {- Logik für das Wechseln der Output-Tabs -}

   let tabClass tab active = if tab == active then "active" else ""
   let activateTab active = do
         if active /= "Schritte"
            then liftIO $ writeIORef traversalRunningStore False
            else return ()
         void $ element outputTabResult # set UI.class_ (tabClass "Ergebnis" active)
         void $ element outputTabLatex # set UI.class_ (tabClass "LaTeX" active)
         void $ element outputTabTree # set UI.class_ (tabClass "Baum" active)
         void $ element outputTabAnalysis # set UI.class_ (tabClass "Analyse" active)
         void $ element outputTabSteps # set UI.class_ (tabClass "Schritte" active)
         void $ element outputTabDetails # set UI.class_ (tabClass "Details" active)
         void $ element outputTabHistory # set UI.class_ (tabClass "Historie" active)
         void $ element outputTabGraph # set UI.class_ (tabClass "Graph" active)
   let refreshResultOverview = do
         result <- liftIO $ readIORef resultStore
         void $ element resultOverview # set UI.html (resultOverviewHtml result)
   let setStatusOk message = do
         void $ element statusIcon # set UI.class_ "status-ok" # set UI.html "&#10003;"
         void $ element statusText # set UI.text message
   let setStatusError message = do
         void $ element statusIcon # set UI.class_ "status-error" # set UI.html "&times;"
         void $ element statusText # set UI.text message
   {-

   Diese Hilfsfunktion wertet das Ergebnis einer GUI-Aktion aus.

   Bei GuiSuccess wird die Statusleiste grün gesetzt.
   Bei GuiError wird die Statusleiste rot gesetzt, das gespeicherte Ergebnis gelöscht und der Darstellungsbereich neutral zurückgesetzt.

   Dadurch muss nicht mehr anhand des Textes im Ausgabebereich geraten werden, ob eine Aktion erfolgreich war.
   Der Handler gibt das Ergebnis direkt als GuiActionResult zurück.

   -}

   let finishResultAction actionResult = do
         case actionResult of
            GuiSuccess message -> setStatusOk message
            GuiError message -> do
               liftIO $ writeIORef resultStore NoResult
               void $ element resultOverview # set UI.html (resultOverviewHtml NoResult)
               void $ element output # set UI.text noValidResultText
               setStatusError message

   {-

   Diese Hilfsfunktion ist für Aktionen der Polynomliste gedacht.

   Auch hier wird GuiActionResult benutzt.
   Anders als bei Rechenfehlern wird resultStore aber nicht geleert, weil ein Fehler beim Hinzufügen oder Löschen
   nicht automatisch ein bereits berechnetes Ergebnis ungültig machen muss.

   -}

   let finishListAction actionResult = do
         case actionResult of
            GuiSuccess message -> setStatusOk message
            GuiError message -> setStatusError message
   refreshPolyList polyStore selectedStore polyListOutput

   {- ActionListener auf die Operation-Buttons: -}

   on UI.click buttonNormalize $ \_ -> do
      actionResult <- handleNormalizeClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonNegate $ \_ -> do
      actionResult <- handleNegateClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonAddPoly $ \_ -> do
      actionResult <- handleAddPolyClick input polyStore selectedStore polyListOutput polyListMessage
      finishListAction actionResult

   on UI.click buttonAdd $ \_ -> do
      actionResult <- handleAddClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonSub $ \_ -> do
      actionResult <- handleSubClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonMult $ \_ -> do
      actionResult <- handleMultClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonDerivation $ \_ -> do
      actionResult <- handleDerivationClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonEvaluate $ \_ -> do
      actionResult <- handleEvaluateClick polyStore selectedStore inputX resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonDiv $ \_ -> do
      actionResult <- handleDivClick polyStore selectedStore resultStore historyStore cacheStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonRandomPoly $ \_ -> do
      actionResult <- handleRandomPolyClick resultStore polyStore selectedStore polyListOutput polyListMessage output
      refreshResultOverview
      finishListAction actionResult

   on UI.click buttonParallel $ \_ -> do
      actionResult <- handleParallelClick polyStore selectedStore inputX resultStore historyStore output
      refreshResultOverview
      finishResultAction actionResult
      activateTab "Ergebnis"

   on UI.click buttonLoadExamples $ \_ -> do
      actionResult <- handleLoadExamplesClick polyStore selectedStore resultStore polyListOutput polyListMessage
      refreshResultOverview
      finishListAction actionResult

   on UI.click clearSelectionButton $ \_ -> do
      actionResult <- handleClearSelectionClick polyStore selectedStore polyListOutput polyListMessage
      finishListAction actionResult

   on UI.click removePolyButton $ \_ -> do
      actionResult <- handleRemovePolyClick polyStore selectedStore resultStore polyListOutput polyListMessage
      finishListAction actionResult

   {- ActionListener auf die Darstellung-Buttons: -}

   on UI.click buttonShowResult $ \_ -> do
      activateTab "Ergebnis"
      actionResult <- handleShowResultClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonShowLatex $ \_ -> do
      activateTab "LaTeX"
      actionResult <- handleLatexClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonTree $ \_ -> do
      activateTab "Baum"
      actionResult <- handleTreeClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonAnalysis $ \_ -> do
      activateTab "Analyse"
      actionResult <- handleAnalysisClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonSteps $ \_ -> do
      activateTab "Schritte"
      actionResult <- handleStepsClick traversalRunningStore resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonDetails $ \_ -> do
      activateTab "Details"
      actionResult <- handleDetailsClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonGraph $ \_ -> do
      activateTab "Graph"
      actionResult <- handleGraphClick resultStore output
      refreshResultOverview
      finishResultAction actionResult

   on UI.click buttonHistory $ \_ -> do
      activateTab "Historie"
      actionResult <- handleHistoryClick historyStore output
      finishResultAction actionResult

{- Polynomlisten-Hilfsfunktionen -}

{- 

Diese Funktion aktualisiert die sichtbare Polynomliste in der GUI.
Sie liest die gespeicherten Polynome aus polyStore und die aktuell ausgewählten Polynome aus selectedStore und zeigt sie in polyListOutput an.

library ist die gesamte Polynomliste, die wir aus polyStore auslesen.
selectedNames ist die Liste der Namen der aktuell ausgewählten Polynome, die wir aus selectedStore auslesen.

Wir prüfen library auf zwei Fälle:

1. Fall: Wenn keine Polynome gespeichert sind, wird ein einfacher Hinweis in der Liste angezeigt.
2. Fall: Wenn Polynome gespeichert sind, wird für jedes Polynom eine eigene Zeile mit Checkbox erzeugt.

Dadurch ist die Polynomliste nicht nur eine Textausgabe, sondern ein interaktiver Bereich, über den man gezielt Polynome auswählen kann.

-}

refreshPolyList :: IORef PolyLibrary -> IORef [String] -> Element -> UI ()
refreshPolyList polyStore selectedStore polyListOutput = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   case library of
      [] -> do
         emptyRow <- UI.div
            # set UI.class_ "poly-list-empty"
            # set UI.text "Noch keine Polynome vorhanden."
         void $ element polyListOutput # set children [emptyRow]
      _ -> do
         rows <- mapM (polyListRow selectedStore selectedNames) library
         void $ element polyListOutput # set children rows

{-

Diese Funktion erzeugt eine einzelne Zeile für die Polynomliste.

Sie bekommt selectedStore, selectedNames und ein einzelnes gespeichertes Polynom (name, poly) übergeben.

selectedStore ist ein IORef, in dem die Namen der aktuell ausgewählten Polynome gespeichert sind.
selectedNames ist eine Liste der Namen der aktuell ausgewählten Polynome, die wir aus selectedStore auslesen.

Eine Zeile besteht aus einer Checkbox, dem Namen des Polynoms und der mathematischen Darstellung des Polynoms.

Wenn die Checkbox angeklickt wird, wird der Name des Polynoms in selectedStore gespeichert oder wieder entfernt.
Die Reihenfolge der ausgewählten Namen entspricht dabei der Klickreihenfolge.
Wir speichern nur den Namen, weil das eigentliche Polynom weiterhin sauber in polyStore liegt.

-}

polyListRow :: IORef [String] -> [String] -> (String, Poly) -> UI Element
polyListRow selectedStore selectedNames (name, poly) = do
   checkbox <- UI.input
      # set (attr "type") "checkbox"
      # set UI.checked (name `elem` selectedNames)

   nameElement <- UI.span
      # set UI.class_ "poly-name"
      # set UI.text name

   polyElement <- UI.span
      # set UI.class_ "poly-value"
      # set UI.text (toPrettyMathPoly poly)

   listRow <- UI.div
      # set UI.class_ "poly-row"
      #+ [element checkbox, element nameElement, element polyElement]

   on UI.click checkbox $ \_ -> do
      checkedNow <- get UI.checked checkbox
      liftIO $ modifyIORef selectedStore $ \selected ->
         if checkedNow
            then if name `elem` selected then selected else selected ++ [name]
            else filter (/= name) selected

   return listRow

{-

Diese Funktion löscht nur die Auswahl, aber nicht die gespeicherten Polynome selbst.
Danach wird die Polynomliste neu gezeichnet, damit keine Checkbox mehr ausgewählt ist.

-}

handleClearSelectionClick :: IORef PolyLibrary -> IORef [String] -> Element -> Element -> UI GuiActionResult
handleClearSelectionClick polyStore selectedStore polyListOutput polyListMessage = do
   liftIO $ writeIORef selectedStore []
   refreshPolyList polyStore selectedStore polyListOutput
   setPolyListSuccess polyListMessage "Auswahl wurde gelöscht."

{-

Diese Funktion wird aufgerufen, wenn "Polynom entfernen" geklickt wird.
Sie entfernt alle ausgewählten Polynome aus der Polynomliste.

Dazu bekommt sie polyStore, selectedStore, polyListOutput und polyListMessage übergeben.

polyStore ist ein IORef, in dem die gesamte Polynomliste gespeichert ist, selectedStore ist ein IORef, 
in dem die Namen der aktuell ausgewählten Polynome gespeichert sind, polyListOutput ist das Element, in dem die Polynomliste angezeigt wird und polyListMessage ist das Element, in dem Fehlermeldungen angezeigt werden können.

Wir prüfen selectedNames auf zwei Fälle:

1. Fall: Wenn nichts ausgewählt wurde, wird eine Fehlermeldung unter der Polynomliste angezeigt.
2. Fall: Wenn Polynome ausgewählt wurden, werden sie aus polyStore entfernt und die Auswahl wird anschließend geleert.

-}

handleRemovePolyClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult-> Element -> Element -> UI GuiActionResult
handleRemovePolyClick polyStore selectedStore resultStore polyListOutput polyListMessage = do
   selectedNames <- liftIO $ readIORef selectedStore
   case selectedNames of
      [] ->
         setPolyListError polyListMessage "Fehler: Bitte wählen Sie mindestens ein Polynom aus."
      _ -> do
         liftIO $ modifyIORef polyStore
            (filter (\(name, _) -> name `notElem` selectedNames))
         liftIO $ writeIORef selectedStore []
         liftIO $ writeIORef resultStore NoResult
         refreshPolyList polyStore selectedStore polyListOutput
         setPolyListSuccess polyListMessage "Ausgewählte Polynome wurden entfernt."

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
set UI.text ein UI.Element zurückgibt, das wir hier aber nicht benötigen.

-}

handleNormalizeClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleNormalizeClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name, poly)] -> normalizeAndShow name poly
      [] ->
         setOutputError output "Fehler: Normalisieren benötigt ein ausgewähltes Polynom. Bitte wählen Sie genau ein Polynom aus."
      other ->
         setOutputError output
            ("Fehler: Normalisieren benötigt genau ein ausgewähltes Polynom. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")
   where
      normalizeAndShow name poly = do
         cache <- liftIO $ readIORef cacheStore
         let operation = Normalize poly
         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Normalisieren" name cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)
            Nothing -> do
               let resultPoly = normalize poly
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Normalisieren" name cachedResult
               let historyText = "Normalisieren von " ++ name
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly historyText resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)
               setOutputSuccess output ("Neu berechnet:\n" ++ resultText)
{- 

Diese Funktion dient zur Veranschaulichung eines negierten Polynoms in der GUI.

Gleiche Logik wie bei handleNormalizeClick, nur dass hier die Funktion negat aufgerufen wird, um das Polynom zu negieren.

Auch hier wird das Ergebnis zusätzlich in resultStore gespeichert.
Das ist wichtig, damit man danach z.B. auf den LaTeX-Button klicken kann, ohne dass nochmal neu gerechnet werden muss.

Da negat wieder ein Polynom zurückgibt, speichern wir das Ergebnis als PolyResult.

-}

handleNegateClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleNegateClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name, poly)] -> negateAndShow name poly
      [] ->
         setOutputError output "Fehler: Negieren benötigt ein ausgewähltes Polynom. Bitte wählen Sie genau ein Polynom aus."
      other ->
         setOutputError output
            ("Fehler: Negieren benötigt genau ein ausgewähltes Polynom. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")
   where
      negateAndShow name poly = do
         cache <- liftIO $ readIORef cacheStore
         let operation = Negate poly
         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Negieren" name cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)
            Nothing -> do
               let resultPoly = negat poly
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Negieren" name cachedResult
               let historyText = "Negieren von " ++ name
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly historyText resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)
               setOutputSuccess output ("Neu berechnet:\n" ++ resultText)

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

Es wird ein Polynom im folgenden Format zum Hinzufügen eingegeben: "3 2; 2 1; 1 0" (Koeffizient Exponent; Koeffizient Exponent; Koeffizient Exponent)
Es wird ein Name automatisch generiert, z.B. p1, p2, p3 usw. und das Polynom wird in der GUI angezeigt (z.B. p1 = 3x^2 + 2x + 1).

-}

handleAddPolyClick :: Element -> IORef PolyLibrary -> IORef [String] -> Element -> Element -> UI GuiActionResult
handleAddPolyClick input polyStore selectedStore polyListOutput polyListMessage = do
   polyStr <- get value input
   let result = parsePolySimple polyStr
   case result of
      Left err ->
         setPolyListError polyListMessage err
      Right poly -> do
         library <- liftIO $ readIORef polyStore
         let name = nextPolyName library
         let newLibrary = savePoly name poly library
         liftIO $ writeIORef polyStore newLibrary
         refreshPolyList polyStore selectedStore polyListOutput
         void $ element input # set value ""
         clearPolyListSuccess polyListMessage "OK: Polynomliste aktualisiert."

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

handleAddClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleAddClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Add poly1 poly2

         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Addieren" (name1 ++ " + " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)

            Nothing -> do
               let resultPoly = add poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Addieren" (name1 ++ " + " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " + " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)

               setOutputSuccess output ("Neu berechnet:\n" ++ resultText)

      [] ->
         setOutputError output "Fehler: Addieren benötigt zwei ausgewählte Polynome. Bitte wählen Sie genau zwei Polynome aus."

      other ->
         setOutputError output
            ("Fehler: Addieren benötigt genau zwei ausgewählte Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")

{- 

Diese Funktion dient zur Veranschaulichung der Subtraktion von zwei Polynomen in der GUI.

Funktioniert genau wie handleAddClick, nur dass hier die Funktion sub aufgerufen wird, um die Subtraktion durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da sub wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handleSubClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleSubClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Sub poly1 poly2
         let orderText = "Reihenfolge: " ++ name1 ++ " - " ++ name2

         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Subtrahieren" (name1 ++ " - " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ orderText ++ "\n" ++ guiResultText guiResult)

            Nothing -> do
               let resultPoly = sub poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Subtrahieren" (name1 ++ " - " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " - " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)

               setOutputSuccess output ("Neu berechnet:\n" ++ orderText ++ "\n" ++ resultText)

      [] ->
         setOutputError output "Fehler: Subtrahieren benötigt zwei ausgewählte Polynome. Bitte wählen Sie genau zwei Polynome aus."

      other ->
         setOutputError output
            ("Fehler: Subtrahieren benötigt genau zwei ausgewählte Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")

{- 

Diese Funktion dient zur Veranschaulichung der Multiplikation von zwei Polynomen in der GUI.

Funktioniert genau wie handleAddClick, nur dass hier die Funktion mult aufgerufen wird, um die Multiplikation durchzuführen.

Auch hier wird das Ergebnis in resultStore gespeichert.
Da mult wieder ein Polynom zurückgibt, benutzen wir PolyResult.

-}

handleMultClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleMultClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name1, poly1), (name2, poly2)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Mul poly1 poly2

         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Multiplizieren" (name1 ++ " * " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)

            Nothing -> do
               let resultPoly = mult poly1 poly2
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly

               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Multiplizieren" (name1 ++ " * " ++ name2) cachedResult
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly (name1 ++ " * " ++ name2) resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)

               setOutputSuccess output ("Neu berechnet:\n" ++ resultText)

      [] -> setOutputError output "Fehler: Multiplizieren benötigt zwei ausgewählte Polynome. Bitte wählen Sie genau zwei Polynome aus."

      other -> setOutputError output ("Fehler: Multiplizieren benötigt genau zwei ausgewählte Polynome. Es wurde/n aber " ++ show (length other) ++ " Polynom/e ausgewählt.")


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

handleDerivationClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleDerivationClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name1, poly1)] -> do
         cache <- liftIO $ readIORef cacheStore
         let operation = Derive poly1
         case lookupCache operation cache of
            Just cachedResult -> do
               let guiResult = cachedToGuiResult "Ableiten" (name1 ++ "'") cachedResult
               liftIO $ writeIORef resultStore guiResult
               setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)
            Nothing -> do
               let resultPoly = derivation poly1
               let resultText = "Ergebnis: " ++ toPrettyMathPoly resultPoly
               let cachedResult = CachedPoly resultPoly
               let guiResult = cachedToGuiResult "Ableiten" (name1 ++ "'") cachedResult
               let historyText = "Ableiten von " ++ name1
               liftIO $ writeIORef resultStore guiResult
               liftIO $ modifyIORef historyStore
                  (addHistory (HistoryPoly historyText resultPoly))
               liftIO $ modifyIORef cacheStore
                  (insertCache operation cachedResult)
               setOutputSuccess output ("Neu berechnet:\n" ++ resultText)
      [] ->
         setOutputError output "Fehler: Ableiten benötigt ein ausgewähltes Polynom. Bitte wählen Sie genau ein Polynom aus."
      other ->
         setOutputError output
            ("Fehler: Ableiten benötigt genau ein ausgewähltes Polynom. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")


{- 

Diese Funktion dient zur Veranschaulichung der Auswertung eines Polynoms an einer bestimmten Stelle in der GUI.

Es wird das polyStore übergeben, um die gespeicherten Polynome zu lesen, das input Element, 
um den Wert für x auszulesen, resultStore, um das Ergebnis zu speichern und das output Element, um das Ergebnis der Auswertung anzuzeigen.

Wir lesen die gespeicherten Polynome aus polyStore und speichern sie in storedPolys.
Wir lesen den Wert für x aus dem input Element aus und speichern ihn in xStr.

Danach prüfen wir für xStr 3 Fälle:
Fall 1: xStr ist leer, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Eingabefeld für xStr ist nicht leer, 
somit gehen wir über in die Prüfung ob es sich um eine gültige Zahl handelt, mithilfe von parseRationalInput, 
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

handleEvaluateClick :: IORef PolyLibrary -> IORef [String] -> Element -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleEvaluateClick polyStore selectedStore input resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   xStr <- get value input
   case xStr of
      "" ->
         setOutputError output "Fehler: Bitte geben Sie einen Wert für x ein."

      _ -> case parseRationalInput xStr :: Maybe Rational of
         Nothing ->
            setOutputError output "Fehler: Bitte geben Sie eine gültige Zahl für x ein."

         Just x -> case selectedLibrary of
            [(name1, poly1)] -> do
               cache <- liftIO $ readIORef cacheStore
               let operation = Evaluate poly1 x

               case lookupCache operation cache of
                  Just cachedResult -> do
                     let inputText = name1 ++ "(" ++ prettyRational x ++ ")"
                     let guiResult = cachedToGuiResult "Auswerten" inputText cachedResult
                     liftIO $ writeIORef resultStore guiResult
                     void $ element input # set value ""
                     setOutputSuccess output ("Aus Cache geladen:\n" ++ guiResultText guiResult)

                  Nothing -> do
                     let resultValue = evaluate poly1 x
                     let resultText = "Ergebnis: " ++ prettyRational resultValue

                     let cachedResult = CachedValue resultValue
                     let inputText = name1 ++ "(" ++ prettyRational x ++ ")"
                     let guiResult = cachedToGuiResult "Auswerten" inputText cachedResult
                     liftIO $ writeIORef resultStore guiResult
                     liftIO $ modifyIORef historyStore
                        (addHistory (HistoryValue inputText resultValue))
                     liftIO $ modifyIORef cacheStore
                        (insertCache operation cachedResult)
                     void $ element input # set value ""

                     setOutputSuccess output ("Neu berechnet:\n" ++ resultText)

            [] ->
               setOutputError output "Fehler: Auswerten benötigt ein ausgewähltes Polynom. Bitte wählen Sie genau ein Polynom aus."

            other ->
               setOutputError output
                  ("Fehler: Auswerten benötigt genau ein ausgewähltes Polynom. Es wurde/n aber "
                   ++ show (length other) ++ " Polynom/e ausgewählt.")

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

handleDivClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> IORef (History HistoryEntry) -> IORef (Cache CachedResult) -> Element -> UI GuiActionResult
handleDivClick polyStore selectedStore resultStore historyStore cacheStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   let selectedLibrary = selectedPolys library selectedNames
   case selectedLibrary of
      [(name1, poly1), (name2, poly2)] -> do
         if normalize poly2 == P []
            then setOutputError output "Fehler: Division durch das Nullpolynom ist nicht erlaubt."
            else do
               cache <- liftIO $ readIORef cacheStore
               let operation = Div poly1 poly2
               let orderText = "Reihenfolge: " ++ name1 ++ " / " ++ name2

               case lookupCache operation cache of
                  Just cachedResult -> do
                     let guiResult = cachedToGuiResult "Dividieren" (name1 ++ " / " ++ name2) cachedResult
                     liftIO $ writeIORef resultStore guiResult
                     setOutputSuccess output ("Aus Cache geladen:\n" ++ orderText ++ "\n" ++ guiResultText guiResult)

                  Nothing -> do
                     let (quotient, rest) = (/%) poly1 poly2
                     let resultText =
                           "Ergebnis: " ++ name1 ++ " / " ++ name2
                           ++ " = " ++ toPrettyMathPoly quotient
                           ++ ", Rest: " ++ toPrettyMathPoly rest

                     let cachedResult = CachedDiv quotient rest
                     let guiResult = cachedToGuiResult "Dividieren" (name1 ++ " / " ++ name2) cachedResult
                     liftIO $ writeIORef resultStore guiResult
                     liftIO $ modifyIORef historyStore
                        (addHistory (HistoryDiv (name1 ++ " / " ++ name2) quotient rest))
                     liftIO $ modifyIORef cacheStore
                        (insertCache operation cachedResult)

                     setOutputSuccess output ("Neu berechnet:\n" ++ orderText ++ "\n" ++ resultText)

      [] ->
         setOutputError output "Fehler: Dividieren benötigt zwei ausgewählte Polynome. Bitte wählen Sie genau zwei Polynome aus."

      other ->
         setOutputError output
            ("Fehler: Dividieren benötigt genau zwei ausgewählte Polynome. Es wurde/n aber "
             ++ show (length other) ++ " Polynom/e ausgewählt.")

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

Wir speichern die neue PolynomBibliothek in polyStore.
resultStore wird geleert, weil ein Zufallspolynom nur eine Eingabe erzeugt und noch kein Rechenergebnis ist.

Zuletzt zeigen wir die neue Polynomliste in polyListOutput an und eine kurze Meldung in output.

-}

handleRandomPolyClick :: IORef GuiResult -> IORef PolyLibrary -> IORef [String] -> Element -> Element -> Element -> UI GuiActionResult
handleRandomPolyClick resultStore polyStore selectedStore polyListOutput polyListMessage output = do
   library <- liftIO $ readIORef polyStore
   poly <- liftIO randomPoly
   let name = nextRandomName library
   let newLibrary = savePoly name poly library
   liftIO $ writeIORef polyStore newLibrary
   {- Das Zufallspolynom ist nur eine neue Eingabe in der Liste und noch kein Ergebnis einer Berechnung. -}
   liftIO $ writeIORef resultStore NoResult
   refreshPolyList polyStore selectedStore polyListOutput
   void $ element output # set UI.text noValidResultText
   void $ element polyListMessage # set UI.text ("Zufallspolynom " ++ name ++ " wurde hinzugefuegt.")
   return (GuiSuccess "OK: Zufallspolynom wurde hinzugefügt.")
   
{- 

Diese Funktion dient wird aufgerufen, wenn der Button "Parallel" geklickt wird.
Sie dient dazu, die Auswertung von mehreren Polynomen an einer bestimmten Stelle parallel durchzuführen.

Sie bekommt polyStore, selectedStore, input, resultStore, historyStore und output übergeben.
polyStore ist der Speicher für die gespeicherten Polynome.
selectedStore enthält die Namen der aktuell ausgewählten Polynome.
input ist das Eingabefeld für den Wert von x und output ist der Ausgabebereich für das Ergebnis.

Danach wird library erstellt, das die aktuelle PolynomBibliothek aus polyStore ausliest und xStr wird erstellt, 
das den Wert von x aus dem Eingabefeld input ausliest.

xStr wird auf zwei Fälle geprüft:
Fall 1: xStr ist leer, dann wird eine Fehlermeldung angezeigt.
Fall 2: xStr ist nicht leer, dann wird geprüft, ob es sich vielleicht um eine gültige 
Zahl (Just x) handelt oder auch nicht (Nothing), mithilfe von parseRationalInput:

Fall 2.1: Es handelt sich nicht um eine gültige Zahl, dann wird eine Fehlermeldung angezeigt.
Fall 2.2: Es handelt sich um eine gültige Zahl, dann prüfen wir die ausgewählten Polynome auf 2 Fälle:

Fall 2.2.1: Es ist kein Polynom ausgewählt, dann wird eine Fehlermeldung angezeigt.
Fall 2.2.2 (Andernfalls): Es sind ein oder mehrere Polynome ausgewählt, dann wird die Auswertung parallel durchgeführt und das Ergebnis angezeigt.

-}

handleParallelClick :: IORef PolyLibrary -> IORef [String] -> Element -> IORef GuiResult -> IORef (History HistoryEntry) -> Element -> UI GuiActionResult
handleParallelClick polyStore selectedStore input resultStore historyStore output = do
   library <- liftIO $ readIORef polyStore
   selectedNames <- liftIO $ readIORef selectedStore
   {- Für Parallel werden nur die per Checkbox ausgewählten Polynome verwendet. -}
   let selectedLibrary = selectedPolys library selectedNames
   xStr <- get value input
   case xStr of
      ""->
         setOutputError output "Fehler: Bitte geben Sie einen Wert für x ein."
      _ -> case parseRationalInput xStr :: Maybe Rational of
         Nothing ->
            setOutputError output "Fehler: Bitte geben Sie eine gültige Zahl für x ein."
         Just x -> do
            case selectedLibrary of
                [] -> setOutputError output "Fehler: Parallel benötigt mindestens ein ausgewähltes Polynom."
                _ -> do
                   let (sequentialResults, parallelResults, sameResult) = compareSequentialAndParallel x selectedLibrary
                   liftIO $ writeIORef resultStore (ParallelResult x parallelResults)
                   liftIO $ modifyIORef historyStore
                      (addHistory (HistoryParallel ("Parallel bei x = " ++ prettyRational x) parallelResults))
                   void $ element input # set value ""
                   setOutputSuccess output (showParallelComparisonText x sequentialResults parallelResults sameResult)

{-

Diese Funktion wird aufgerufen, wenn der Button "Beispiele laden" geklickt wird.

Die Beispielpolynome kommen aus Examples.hs.
Dort werden sie mit Template Haskell zur Compile-Zeit erzeugt.

In der GUI sieht der Benutzer davon eine normale Polynomliste.
Der fachliche Punkt ist aber:
Die Polynome wurden nicht zur Laufzeit aus einem String geparst, sondern bereits beim Kompilieren aus einer kompakten Beschreibung erzeugt.

polyStore speichert die gesamte Polynomliste.
selectedStore speichert die aktuell ausgewählten Polynome.
resultStore wird geleert, weil durch das Laden neuer Beispiele ein altes Ergebnis nicht mehr zur aktuellen Polynomliste passen muss.
polyListOutput ist der sichtbare Bereich der Polynomliste.
polyListMessage zeigt die Erfolgsmeldung unter der Polynomliste an.

Wenn bereits Beispielpolynome mit denselben Namen vorhanden sind, werden sie ersetzt.
Andere Polynome, die der Benutzer selbst hinzugefügt hat, bleiben erhalten.

-}

handleLoadExamplesClick :: IORef PolyLibrary -> IORef [String] -> IORef GuiResult -> Element -> Element -> UI GuiActionResult
handleLoadExamplesClick polyStore selectedStore resultStore polyListOutput polyListMessage = do
   library <- liftIO $ readIORef polyStore
   let exampleNames = map fst exampleLibrary
   let userLibrary = filter (\(name, _) -> name `notElem` exampleNames) library
   let newLibrary = exampleLibrary ++ userLibrary
   liftIO $ writeIORef polyStore newLibrary
   liftIO $ writeIORef selectedStore []
   liftIO $ writeIORef resultStore NoResult
   refreshPolyList polyStore selectedStore polyListOutput
   setPolyListSuccess polyListMessage "Template-Haskell-Beispiele wurden geladen."
             
{- Darstellungshandler -}

{- 

Diese Funktion errechnet nichts neu, sondern zeigt das Ergebnis als LaTeX in der GUI an, wenn der Button "LaTeX" geklickt wird.
Wir lesen das Ergebnis einer Berechnung aus resultStore aus und prüfen vier Fälle:

Fall 1: Es gibt kein Ergebnis, dann wird eine Fehlermeldung angezeigt.
Fall 2: Das Ergebnis ist ein Polynom, dann wird das Polynom in LaTeX angezeigt.
Fall 3: Das Ergebnis ist ein Wert, dann wird der Wert in LaTeX angezeigt
Fall 4: Das Ergebnis ist eine Division von zwei Polynomen, dann wird der Quotient und der Rest in LaTeX angezeigt.

-}

handleLatexClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleLatexClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly ->
         setOutputSuccess output
            ("LaTeX von " ++ name ++ ": " ++ toLaTeX poly)
      ValueResult _ name resultValue ->
         setOutputSuccess output
            ("LaTeX von " ++ name ++ ": " ++ toLaTeX resultValue)
      DivResult _ name quotient rest ->
         setOutputSuccess output
            ("LaTeX von " ++ name ++ ": Quotient = "
             ++ toLaTeX quotient ++ ", Rest = " ++ toLaTeX rest)
      ParallelResult x results ->
         setOutputSuccess output
            ("LaTeX von paralleler Auswertung bei x = " ++ toLaTeX x ++ ":\n"
             ++ unlines [resultName ++ " = " ++ toLaTeX resultValue | (resultName, resultValue) <- results])

{- 

Diese Funktion wird aufgerufen, wenn der Button "Ergebnis" geklickt wird.
Sie dient dazu, das Ergebnis einer Berechnung mathematisch anzuzeigen.

Gleiches Vorgehen wie bei handleLatexClick, nur dass hier die mathematische Darstellung (toPrettyMathPoly und toPrettyMathRational) verwendet wird,
um das Ergebnis in einer mathematischen Form anzuzeigen, die für den Benutzer leichter verständlich ist.

-}

handleShowResultClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleShowResultClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly -> setOutputSuccess output (showResultText name poly)
      ValueResult _ name resultValue -> setOutputSuccess output (showValueText name resultValue)
      DivResult _ name quotient rest ->
         setOutputSuccess output
            (showDivResultText name quotient rest)
      ParallelResult x results ->
         setOutputSuccess output (showParallelResultsText x results)

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

handleTreeClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleTreeClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult -> setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly -> do
         setOutputSuccess output (showTreeText name poly)
      ValueResult _ name resultValue -> do
         setOutputSuccess output (showValueTreeText name resultValue)
      DivResult _ name quotient rest -> do
         setOutputSuccess output (showDivTreeText name quotient rest)
      ParallelResult x results ->
         setOutputSuccess output
            ("Baumdarstellung der parallelen Auswertung bei x = "
             ++ prettyRational x ++ ":\n"
             ++ unlines [resultName ++ " -> " ++ prettyTree (TConst resultValue) | (resultName, resultValue) <- results])

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

handleAnalysisClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleAnalysisClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult -> setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly -> do
         setOutputSuccess output (showAnalysisText name poly)
      ValueResult _ name resultValue -> do
         setOutputSuccess output (showValueAnalysisText name resultValue)
      DivResult _ name quotient rest -> do
         setOutputSuccess output (showDivAnalysisText name quotient rest)
      ParallelResult x results ->
         setOutputSuccess output
            ("Analyse der parallelen Auswertung bei x = "
             ++ prettyRational x ++ ":\n"
             ++ "Anzahl ausgewerteter Polynome: " ++ show (length results) ++ "\n"
             ++ unlines [resultName ++ ": Wert = " ++ prettyRational resultValue | (resultName, resultValue) <- results])


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

startTraversalAnimation :: IORef Bool -> ExprTree -> [TraversalStep] -> Element -> UI ()
startTraversalAnimation traversalRunningStore tree steps output = do
   liftIO $ writeIORef traversalRunningStore True
   stepStore <- liftIO $ newIORef 0

   timer <- UI.timer # set UI.interval 700

   on UI.tick timer $ \_ -> do
      isRunning <- liftIO $ readIORef traversalRunningStore
      if not isRunning
         then do
            UI.stop timer
            return ()
         else do
            currentIndex <- liftIO $ readIORef stepStore
            if currentIndex >= length steps
               then do
                  liftIO $ writeIORef traversalRunningStore False
                  UI.stop timer
                  void $ setOutputSuccess output (showStepsText tree steps currentIndex)
               else do
                  void $ setOutputSuccess output (showStepsText tree steps currentIndex)
                  liftIO $ writeIORef stepStore (currentIndex + 1)

   UI.start timer

{-

Diese Funktion startet die Animation für eine Polynomdivision.

Bei einer Division gibt es zwei Ergebnisbäume:
1. Den Baum für den Quotienten.
2. Den Baum für den Rest.

Deshalb wird zuerst der Quotient Schritt für Schritt traversiert.
Danach wird der Rest Schritt für Schritt traversiert.

-}

startDivisionTraversalAnimation :: IORef Bool -> String -> ExprTree -> [TraversalStep] -> ExprTree -> [TraversalStep] -> Element -> UI ()
startDivisionTraversalAnimation traversalRunningStore inputText quotientTree quotientSteps restTree restSteps output = do
   liftIO $ writeIORef traversalRunningStore True
   {- phaseStore merkt sich, ob gerade der Quotient oder der Rest animiert wird. -}
   phaseStore <- liftIO $ newIORef ("Quotient" :: String)
   {- stepStore merkt sich den aktuellen Traversierungsschritt innerhalb der aktuellen Phase. -}
   stepStore <- liftIO $ newIORef 0

   timer <- UI.timer # set UI.interval 700

   on UI.tick timer $ \_ -> do
      isRunning <- liftIO $ readIORef traversalRunningStore
      if not isRunning
         then do
            UI.stop timer
            return ()
         else do
            phase <- liftIO $ readIORef phaseStore
            currentIndex <- liftIO $ readIORef stepStore
            case phase of
               "Quotient" ->
                  if currentIndex >= length quotientSteps
                     then do
                        liftIO $ writeIORef phaseStore "Rest"
                        liftIO $ writeIORef stepStore 0
                     else do
                        void $ setOutputSuccess output
                           ("Schritte von " ++ inputText ++ ":\n\n"
                            ++ "Quotient:\n"
                            ++ showStepsText quotientTree quotientSteps currentIndex)
                        liftIO $ writeIORef stepStore (currentIndex + 1)
               _ ->
                  if currentIndex >= length restSteps
                     then do
                        liftIO $ writeIORef traversalRunningStore False
                        UI.stop timer
                        void $ setOutputSuccess output
                           ("Schritte von " ++ inputText ++ ":\n\n"
                            ++ showStepsOverviewText "Quotient:" quotientTree quotientSteps
                            ++ "\n"
                            ++ showStepsOverviewText "Rest:" restTree restSteps
                            ++ "\nTraversierung abgeschlossen.")
                     else do
                        void $ setOutputSuccess output
                           ("Schritte von " ++ inputText ++ ":\n\n"
                            ++ "Rest:\n"
                            ++ showStepsText restTree restSteps currentIndex)
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

handleStepsClick :: IORef Bool -> IORef GuiResult -> Element -> UI GuiActionResult
handleStepsClick traversalRunningStore resultStore output = do
   isRunning <- liftIO $ readIORef traversalRunningStore
   if isRunning
      then return (GuiSuccess "OK: Traversierung laeuft bereits.")
      else runStepsClick traversalRunningStore resultStore output

runStepsClick :: IORef Bool -> IORef GuiResult -> Element -> UI GuiActionResult
runStepsClick traversalRunningStore resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ _ poly -> do
         let tree = polyToExprTree poly
         let traversal = preOrder tree
         let steps = makeTraversalSteps traversal
         startTraversalAnimation traversalRunningStore tree steps output
         return (GuiSuccess "OK: Berechnung erfolgreich.")
      ValueResult _ _ resultValue -> do
         let tree = TConst resultValue
         let traversal = preOrder tree
         let steps = makeTraversalSteps traversal
         startTraversalAnimation traversalRunningStore tree steps output
         return (GuiSuccess "OK: Berechnung erfolgreich.")
      DivResult _ inputText quotient rest -> do
         let quotientTree = polyToExprTree quotient
         let restTree = polyToExprTree rest
         let quotientSteps = makeTraversalSteps (preOrder quotientTree)
         let restSteps = makeTraversalSteps (preOrder restTree)
         startDivisionTraversalAnimation traversalRunningStore inputText quotientTree quotientSteps restTree restSteps output
         return (GuiSuccess "OK: Berechnung erfolgreich.")
      ParallelResult x results ->
         setOutputSuccess output
            ("Schritte der parallelen Auswertung bei x = "
             ++ prettyRational x ++ ":\n"
             ++ "Die Polynome wurden unabhängig voneinander parallel ausgewertet.\n"
             ++ unlines [resultName ++ " -> " ++ prettyRational resultValue | (resultName, resultValue) <- results])

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

handleDetailsClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleDetailsClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly ->
         setOutputSuccess output (showDetailsText name poly)
      ValueResult _ name resultValue ->
         setOutputSuccess output (showValueDetailsText name resultValue)
      DivResult _ name quotient rest ->
         setOutputSuccess output (showDivDetailsText name quotient rest)
      ParallelResult x results ->
         setOutputSuccess output
            ("Details der parallelen Auswertung:\n"
             ++ "x-Wert: " ++ prettyRational x ++ "\n"
             ++ "Anzahl Polynome: " ++ show (length results) ++ "\n"
             ++ showParallelResultsText x results)

{- 

Diese Funktion wird aufgerufen, wenn der Button "Graph" geklickt wird.
Sie dient dazu, den Graphen eines Polynoms anzuzeigen.

Funktioniert ähnlich wie die anderen Darstellungsfunktionen, nur dass hier die Funktion graphView 
aufgerufen wird, um den Graphen des Polynoms zu erstellen.

Ebenfalls nutzen wir bei einer gültigen Ausgabe eines FunktionsGraphen UI.html anstatt UI.text, 
da wir hier HTML-Code zurückgeben, um den Graphen in der GUI anzuzeigen.

-}

handleGraphClick :: IORef GuiResult -> Element -> UI GuiActionResult
handleGraphClick resultStore output = do
   result <- liftIO $ readIORef resultStore
   case result of
      NoResult ->
         setOutputError output "Fehler: Es wurde noch kein Ergebnis berechnet."
      PolyResult _ name poly -> do
         setOutputHtmlSuccess output (showGraphText name poly)
      ValueResult _ name resultValue -> do
         setOutputHtmlSuccess output (showValueGraphText name resultValue)
      DivResult _ name quotient rest -> do
         setOutputHtmlSuccess output (showGraphDivText name quotient rest)
      ParallelResult x results ->
         setOutputSuccess output
            ("Die parallele Auswertung liefert einzelne Werte bei x = "
             ++ prettyRational x
             ++ " und keinen eigenen Funktionsgraphen.\n"
             ++ showParallelResultsText x results)

{- 

Diese Funktion wird aufgerufen, wenn der Button "Historie" geklickt wird.
Sie dient dazu, die History der Ergebnisse von Berechnungen anzuzeigen.

Sie bekommt die History aus historyStore und den Ausgabebereich output übergeben.
Es wird "history" erstellt, um die gespeicherte History aus historyStore zu lesen und auf 2 Fälle zu prüfen:

1. Fall: Die History ist leer, dann wird eine Fehlermeldung angezeigt.
2. Fall: Die History enthält Einträge, dann werden diese mithilfe showHistoryText angezeigt.

-}

handleHistoryClick :: IORef (History HistoryEntry) -> Element -> UI GuiActionResult
handleHistoryClick historyStore output = do
   history <- liftIO $ readIORef historyStore
   case history of
      Empty -> setOutputError output "Fehler: Es wurde noch keine Historie erstellt."
      _ -> setOutputSuccess output (showHistoryText history)
