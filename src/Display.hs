module Display where

import Analysis (analyzeTree)
import Animation (TraversalStep(..), showTraversalStep)
import Format 
import Poly
import Tree (ExprTree(..), polyToExprTree, prettyTree, prettyTreeMarked)

{- 

Hier kommt die Logik für die Ausgaben rein, die später in der GUI angezeigt werden.
Das Modul baut also nur die fertigen Strings zusammen und hält die Anzeige-Logik getrennt von der GUI.

-}

{- Ergebnis-Darstellung -}

{- 

Diese Funktion stellt das Ergebnis für ein Polynom dar.

-}
showResultText :: String -> Poly -> String
showResultText name poly = "Ergebnis von " ++ name ++ ": " ++ toPrettyMathPoly poly

{- Diese Funktion stellt das Ergebnis für einen Wert dar.-}
showValueText :: String -> Rational -> String
showValueText name value = "Ergebnis von " ++ name ++ ": " ++ prettyRational value

{- Diese Funktion stellt das Ergebnis für eine Division dar. -}
showDivResultText :: String -> Poly -> Poly -> String
showDivResultText name quotient rest =
   "Ergebnis von " ++ name ++ ": Quotient = " ++ toPrettyMathPoly quotient ++ ", Rest = " ++ toPrettyMathPoly rest

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

{- Diese Funktion stellt die Detailansicht für ein Polynom dar. -}

showDetailsText :: String -> Poly -> String
showDetailsText name poly =
   "Details von " ++ name ++ ":\n"
   ++ "Polynom: " ++ toPrettyMathPoly poly ++ "\n"
   ++ "LaTeX: " ++ toLaTeX poly ++ "\n"
   ++ "Baum:\n" ++ prettyTree (polyToExprTree poly)

{- Diese Funktion stellt die Detailansicht für einen einzelnen Wert dar. -}

showValueDetailsText :: String -> Rational -> String
showValueDetailsText name value =
   "Details von " ++ name ++ ":\n"
   ++ "Wert: " ++ prettyRational value ++ "\n"
   ++ "LaTeX: " ++ toLaTeX value ++ "\n"
   ++ "Baum:\n" ++ prettyTree (TConst value)

{- Diese Funktion stellt die Detailansicht für eine Division mit Quotient und Rest dar. -}

showDivDetailsText :: String -> Poly -> Poly -> String
showDivDetailsText name quotient rest =
   "Details von " ++ name ++ ":\n"
   ++ "Quotient: " ++ toPrettyMathPoly quotient ++ "\n"
   ++ "Rest: " ++ toPrettyMathPoly rest ++ "\n"
   ++ "LaTeX Quotient: " ++ toLaTeX quotient ++ "\n"
   ++ "LaTeX Rest: " ++ toLaTeX rest ++ "\n"
   ++ "Baum Quotient:\n" ++ prettyTree (polyToExprTree quotient) ++ "\n"
   ++ "Baum Rest:\n" ++ prettyTree (polyToExprTree rest)


{- 

Diese Funktion soll die Darstellung einer MonomListe als String ermöglichen, 
die dann in der GUI unter dem Reiter Detaiks angezeigt werden kann.

Die Funktion bekommt als Eingabe ein Polynom, z.B. P [M 3 2, M 2 1, M 1 0] 
und gibt als Ausgabe einen String zurück, z.B. "[(3,2),(2,1),(1,0)]".

-}

showMonomList :: Poly -> String
showMonomList poly = let P ms = normalize poly
    in show [(k,e) | M k e <- ms]

{- Diese Funktion stellt die Darstellung eines Polynoms in Haskell-Format (interne Darstellung) dar. -}

showPolyInHaskell :: Poly -> String
showPolyInHaskell poly = show (normalize poly)

{- Diese Funktion stellt ein Polynom als einen Ausdrucksbaum dar. -}

toAstView :: Poly -> String
toAstView poly = show (polyToExprTree poly)




