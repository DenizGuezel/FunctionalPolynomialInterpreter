module Display where

import Analysis (analyzeTree)
import Animation (TraversalStep(..), showTraversalStep)
import Format 
import Poly
import Tree (ExprTree(..), polyToExprTree, prettyTree, prettyTreeMarked)
import Graph
import History (History(..), HistoryEntry(..), historyToList)
import Library (PolyLibrary)

{- 

Hier kommt die Logik für die Ausgaben rein, die später in der GUI angezeigt werden.
Das Modul baut also nur die fertigen Strings zusammen und hält die Anzeige-Logik getrennt von der GUI.

-}

{- Ergebnis-Darstellung -}

{- Diese Funktion stellt das Ergebnis für ein Polynom dar. -}

showResultText :: String -> Poly -> String
showResultText name poly = "Ergebnis von " ++ name ++ ": " ++ pretty poly

{- Diese Funktion stellt das Ergebnis für einen Wert dar.-}

showValueText :: String -> Rational -> String
showValueText name value = "Ergebnis von " ++ name ++ ": " ++ pretty value

{- Diese Funktion stellt das Ergebnis für eine Division dar. -}

showDivResultText :: String -> Poly -> Poly -> String
showDivResultText name quotient rest =
   "Ergebnis von " ++ name ++ ": Quotient = " ++ pretty quotient ++ ", Rest = " ++ pretty rest

{- Baum-Darstellung -}

{- Diese Funktion stellt den Baum für ein Polynom dar. -}

showTreeText :: String -> Poly -> String
showTreeText name poly = "Baum von " ++ name ++ ":\n" ++ prettyTree (polyToExprTree poly)

{- Diese Funktion stellt den Baum für einen einzelnen Wert dar. -}

showValueTreeText :: String -> Rational -> String
showValueTreeText name value = "Baum von " ++ name ++ ":\n" ++ prettyTree (TConst value)

{- Diese Funktion stellt den Baum für eine Division mit Quotient und Rest dar. -}

showDivTreeText :: String -> Poly -> Poly -> String
showDivTreeText name quotient rest =
   "Baum von " ++ name ++ ":\nQuotient:\n" ++ prettyTree (polyToExprTree quotient) ++ "\nRest:\n" ++ prettyTree (polyToExprTree rest)

{- Analyse-Darstellung -}

{- Diese Funktion stellt die Analyse für ein Polynom dar. -}

showAnalysisText :: String -> Poly -> String
showAnalysisText name poly = "Analyse von Baum " ++ name ++ ":\n" ++ analyzeTree (polyToExprTree poly)

{- Diese Funktion stellt die Analyse für einen einzelnen Wert dar. -}

showValueAnalysisText :: String -> Rational -> String
showValueAnalysisText name value = "Analyse von Baum " ++ name ++ ":\n" ++ analyzeTree (TConst value)

{- Diese Funktion stellt die Analyse für eine Division mit Quotient und Rest dar. -}

showDivAnalysisText :: String -> Poly -> Poly -> String
showDivAnalysisText name quotient rest =
   "Analyse von Baum " ++ name ++ ":\nQuotient:\n" ++ analyzeTree (polyToExprTree quotient) ++ "\nRest:\n" ++ analyzeTree (polyToExprTree rest)

{- Traversierung / Schritte -}

{- Diese Funktion stellt die Ausgabe der Traversierungsschritte für einen Baum dar. -}

showStepsText :: ExprTree -> [TraversalStep] -> Int -> String
showStepsText tree steps currentIndex
   | currentIndex >= length steps = "Baum:\n" ++ prettyTree tree ++ "\nTraversierung abgeschlossen."
   | otherwise =
      let currentStep = steps !! currentIndex
          TraversalStep stepnumber _ _ = currentStep
      in "Baum:\n" ++ prettyTreeMarked stepnumber tree ++ "\n" ++ showTraversalStep currentStep

{- Detail-Darstellung -}

{-

Diese Funktion stellt die verschiedenen Darstellungen eines Polynoms dar. 
Sie dient als Wiederverwendbare Funktion, um die Details eines Polynoms in der GUI für das richtige GuiResult anzuzeigen.

-}

showPolyRepresentations :: String -> Poly -> String
showPolyRepresentations name poly =
   let normalizedPoly = normalize poly
       tree = polyToExprTree normalizedPoly
   in name ++ ":\n"
      ++ "Mathematisch: " ++ pretty normalizedPoly ++ "\n"
      ++ "Monomliste: " ++ showMonomListNormalized normalizedPoly ++ "\n"
      ++ "Interne Haskell-Darstellung: " ++ show normalizedPoly ++ "\n"
      ++ "LaTeX: " ++ polyToLaTeX normalizedPoly ++ "\n"
      ++ "AST: " ++ show tree ++ "\n"
      ++ "Baum:\n" ++ prettyTree tree


{- Diese Funktion stellt die Detailansicht für einen einzelnen Wert dar. -}

showValueDetailsText :: String -> Rational -> String
showValueDetailsText name value =
   "Darstellungen von " ++ name ++ ":\n"
   ++ "Wert: " ++ pretty value ++ "\n"
   ++ "Haskell-Darstellung: " ++ show value ++ "\n"
   ++ "LaTeX: " ++ toLaTeX value ++ "\n"
   ++ "Baum:\n" ++ prettyTree (TConst value)

{- Diese Funktion stellt die Detailansicht für eine Division mit Quotient und Rest dar. -}

showDivDetailsText :: String -> Poly -> Poly -> String
showDivDetailsText name quotient rest =
   "Darstellungen von " ++ name ++ ":\n\n"
   ++ showPolyRepresentations "Quotient" quotient
   ++ "\n"
   ++ showPolyRepresentations "Rest" rest

{-

Diese Funktion stellt die Detailansicht für ein Polynom dar. 
Wir verwenden diese Funktion, um die Details eines Polynoms in der GUI anzuzeigen, also sobald der Benutzer auf den Button "Details" klickt, 
wird mithilfe des handlers diese Funktion aufgerufen.

-}

showDetailsText :: String -> Poly -> String
showDetailsText name poly =
   "Darstellungen von " ++ name ++ ":\n"
   ++ showPolyRepresentations "Polynom" poly

{- 

Diese Funktion soll die Darstellung einer MonomListe als String ermöglichen, 
die dann in der GUI unter dem Reiter Detaiks angezeigt werden kann.

Die Funktion wird auf bereits normalisierte Polynome angewendet, 
um die Monomliste in einer standardisierten Form darzustellen.

Die Funktion bekommt als Eingabe ein Polynom, z.B. P [M 3 2, M 2 1, M 1 0] 
und gibt als Ausgabe einen String zurück, z.B. "[(3,2),(2,1),(1,0)]".

-}

showMonomListNormalized :: Poly -> String
showMonomListNormalized (P ms) = show [(k,e) | M k e <- ms]

{- Graph-Darstellung -}

{- Diese Funktion stellt die Graph-Darstellung eines Polynoms dar. -}

showGraphText :: String -> Poly -> String
showGraphText name poly = "<h3>Graph von " ++ name ++ ":</h3>\n" ++ graphView poly

{- Diese Funktion stellt die Graph-Darstellung eines einzelnen Wertes dar. -}

showValueGraphText :: String -> Rational -> String
showValueGraphText name value =
   "Fehler: " ++ name ++ " ist ein einzelner Wert (" ++ pretty value ++ ") und hat keinen Funktionsgraphen."

{- Diese Funktion stellt die Graph-Darstellung einer Division mit Quotient und Rest dar. -}
showGraphDivText :: String -> Poly -> Poly -> String
showGraphDivText name quotient rest = 
   "<h3>Graph von " ++ name ++ ":</h3>\n" ++ graphView quotient ++ "<br><br>" ++ graphView rest

{- Historie-Darstellung -}

{- Diese Hauptfunktion stellt die ganze Historie der Ergebnisse als einen schön formartierten String dar. -}

showHistoryText :: History HistoryEntry-> String
showHistoryText history =
   case historyToList history of
      [] -> "Historie ist leer."
      entries -> "Historie:\n" ++ unlines (zipWith formatEntry [1..] entries)

{- Diese Hilfsfunktion formatiert einen Eintrag, je nach HistoryEntry-Typ in der Historie in einen schön formartierten String. -}

formatEntry :: Int -> HistoryEntry -> String
formatEntry n (HistoryPoly name poly) =
   show n ++ ". " ++ name ++ ": " ++ pretty poly
formatEntry n (HistoryValue name value) =
   show n ++ ". " ++ name ++ ": " ++ pretty value
formatEntry n (HistoryDiv name quotient rest) =
   show n ++ ". " ++ name
   ++ ": Quotient = " ++ pretty quotient
   ++ ", Rest = " ++ pretty rest

{- Library-Darstellung -}

{- Diese Funktion stellt jeden Eintrag aus der PolyLibrary als einen String im Fromat: <polynomname> = <polynom> dar. -}
showPolyLibraryText :: PolyLibrary -> String
showPolyLibraryText [] = "Noch keine Polynome vorhanden."
showPolyLibraryText library = unlines [name ++ " = " ++ pretty poly | (name, poly) <- library]

{- Random-Darstellung -}

{- 

Diese Funktion stellt ein zufällig erzeugtes Polynom als String dar. 
Wir brauchen es nicht noch für ValueResult oder DivResult, da wir nur zufällige Polynome erzeugen wollen, 
die wir dann in der GUI anzeigen.

Der Zufallspolynom-Button in der GUI erzeugt keine Divisiom und keinen einzelnen Wert.
Division und Auswertung entstehen erst, wenn der Benutzer die entsprechenden Buttons für die Operationen klickt.

-}

showRandomPolyText :: String -> Poly -> String
showRandomPolyText name poly = "Zufallspolynom " ++ name ++ ": " ++ pretty poly

{- Parallel-Darstellung -}

{- Diese Funktion stellt die Ergebnisse einer parallelen Auswertung als String dar. -}
showParallelResultsText :: Rational -> [(String, Rational)] -> String
showParallelResultsText x results = 
   "Parallele Auswertung bei x = " ++ pretty x ++ ":\n" 
   ++ unlines [name ++ ": " ++ pretty value | (name, value) <- results]

{-

Diese Funktion stellt die parallele Auswertung zusammen mit einem Vergleich zur sequentiellen Auswertung dar.

sequentialResults sind die Ergebnisse aus der normalen map-Version.
parallelResults sind die Ergebnisse aus der parMap-Version.
sameResult sagt, ob beide Ergebnislisten gleich sind.

Dadurch sieht man in der GUI nicht nur das Ergebnis, sondern auch, dass die parallele Version fachlich dasselbe liefert.

-}

showParallelComparisonText :: Rational -> [(String, Rational)] -> [(String, Rational)] -> Bool -> String
showParallelComparisonText x sequentialResults parallelResults sameResult =
   "Parallele Auswertung bei x = " ++ pretty x ++ ":\n"
   ++ unlines [name ++ ": " ++ pretty value | (name, value) <- parallelResults]
   ++ "\nVergleich mit sequentieller Auswertung:\n"
   ++ unlines [name ++ ": " ++ pretty value | (name, value) <- sequentialResults]
   ++ "\nKorrektheitscheck: "
   ++ (if sameResult then "parallel und sequentiell liefern dasselbe Ergebnis." else "parallel und sequentiell unterscheiden sich.")
   ++ "\n\nKonzept:\n"
   ++ "Jedes Polynom wird unabhängig an derselben Stelle x ausgewertet.\n"
   ++ "Deshalb kann die Liste der Polynome mit parMap rdeepseq parallel verarbeitet werden."
