module Format where

import Data.Ratio (denominator, numerator)
import Poly

{- 

Hier kommt die Logik für die Formatierung von Polynomen, Bäumen und Traversierungen rein, die später in der GUI angezeigt werden.
Anders als im Modul Display werden hier die Hilfsfunktionen definiert, die die Formatierung übernehmen,
während im Modul Display die fertigen Strings zusammengebaut werden, die dann in der GUI angezeigt werden.

-}

{- Zahlenformate / Polynomformate -}

{- 

Diese Hilfsfunktion wandelt eine rationale Zahl in eine lesbare Form um.
demoniator ist der Nenner der rationalen Zahl, numerator ist der Zähler.

Wenn der Nenner 1 ist, wird nur der Zähler als String zurückgegeben (z.B 3 % 1 wird als "3" dargestellt, da 3 % 1 = 3/1 = 3 ist), 
ansonsten wird der Zähler und der Nenner durch einen Bruchstrich getrennt zurückgegeben (z.B 3 % 2 wird als "3/2" dargestellt, da 3 % 2 = 3/2 ist).

-}

prettyRational :: Rational -> String
prettyRational r
   | denominator r == 1 = show (numerator r)
   | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

{- 

Diese Hilfsfunktion stellt eine Varibale (z.B x^2) mithilfe von prettyExponent in einer lesbaren Form dar.
z.B wird die Eingabe 2 als "x²" dargestellt.

-}

prettyVariable :: Int -> String
prettyVariable 1 = "x"
prettyVariable e = "x" ++ prettyExponent e

{- 

Diese Hilfsfunktion soll einen Exponenten in eine lesbare Form umwandeln.
Je nach Exponent wird eine andere Darstellung gewählt, z.B. 2 wird als "²" dargestellt.

-}

prettyExponent :: Int -> String
prettyExponent 0 = ""
prettyExponent 1 = ""
prettyExponent 2 = "²"
prettyExponent 3 = "³"
prettyExponent e = "^" ++ show e

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

{- 

Diese Hilfsfunktion soll ein Polynom in eine mathematische Form bringen, die für den Benutzer leichter verständlich ist.
Sie bekommt als Eingabe ein Polynom, z.B. P [M 3 2, M 2 1, M 1 0] und gibt als Ausgabe einen String zurück, z.B. "3x² + 2x + 1".

-}

prettyPoly :: Poly -> String
prettyPoly (P []) = "0"
prettyPoly (P (m:ms)) = prettyMonomFirst m ++ prettyMonomRest ms

{- 

Diese Hilfsfunktion soll die Restmonome eines Polynoms in eine mathematische Form bringen, die für den Benutzer leichter verständlich ist.
Sie bekommt als Eingabe eine Liste von Monomen, z.B. [M 2 1, M 1 0] und gibt als Ausgabe einen String zurück, z.B. " + 2x + 1".

-}

prettyMonomRest :: [Monom] -> String
prettyMonomRest [] = ""
prettyMonomRest (m:ms) = prettyMonomWithSign m ++ prettyMonomRest ms

{- 

Diese Hilfsfunktion soll einen Monom in eine mathematische Form bringen, die für den Benutzer leichter verständlich ist.
Sie bekommt als Eingabe ein Monom, z.B. M 2 1 und gibt als Ausgabe einen String zurück, z.B. "2x".

Diese Hilfsmethode betrachtet ebenfalls das Vorzeichen des Koeffizienten und gibt das Monom mit einem "+" oder "-" zurück, 
je nachdem ob der Koeffizient positiv oder negativ ist.

-}

prettyMonomWithSign :: Monom -> String
prettyMonomWithSign (M k e)
   | k >= 0 =
      " + " ++ prettyMonom (M k e)
   | otherwise =
      " - " ++ prettyMonom (M (-k) e)

{- 

Diese Hilfsfunktion soll den ersten Term eines Polynoms in eine mathematische Form bringen, die für den Benutzer leichter verständlich ist.
Sie bekommt als Eingabe ein Monom, z.B. M 3 2 und gibt als Ausgabe einen String zurück, z.B. "3x²".

-}

prettyMonomFirst :: Monom -> String
prettyMonomFirst (M k e)
   | k < 0 =
      "-" ++ prettyMonom (M (-k) e)
   | otherwise =
      prettyMonom (M k e)

{- 

Diese Hilfsfunktion soll ein komplettes Monom in eine mathematische Form bringen, die für den Benutzer leichter verständlich ist.
Sie bekommt als Eingabe ein Monom, z.B. M 2 1 und gibt als Ausgabe einen String zurück, z.B. "2x".

-}

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


{- Baumformate -}

{- 

Diese Hilfsfunktion soll die Kindknoten eines Ausdrucksbaums in einer lesbaren Form darstellen.

Wenn die Liste der Kindknoten leer ist, wird ein leerer String zurückgegeben.
Wenn die Liste der Kindknoten genau ein Element enthält, wird dieses Element mit einem "`-- " Präfix dargestellt.
Wenn die Liste der Kindknoten mehr als ein Element enthält, wird das erste Element mit einem "|-- " Präfix dargestellt und 
die restlichen Elemente werden rekursiv mit einem "|   " Präfix dargestellt. 

-}

prettyTreeWith :: (a -> String) -> (a -> [a]) -> a -> String
prettyTreeWith label children tree = label tree ++ "\n" ++ prettyChildrenWith label children "" (children tree)
prettyChildrenWith :: (a -> String) -> (a -> [a]) -> String -> [a] -> String
prettyChildrenWith _ _ _ [] = ""
prettyChildrenWith label children prefix [child] =
   prefix ++ "`-- " ++ label child ++ "\n"
   ++ prettyChildrenWith label children (prefix ++ "    ") (children child)
prettyChildrenWith label children prefix (child:rest) =
   prefix ++ "|-- " ++ label child ++ "\n"
   ++ prettyChildrenWith label children (prefix ++ "|   ") (children child)
   ++ prettyChildrenWith label children prefix rest

{- 

Diese Funktion soll einen Ausdrucksbaum als einen String zurückgeben, der den aktuellen Knoten markiert, der gerade besucht wird.
Sie bekommt als Eingabe die aktuelle Schrittnummer und den Ausdrucksbaum und gibt als Ausgabe einen String zurück, der den Ausdrucksbaum in einer lesbaren Form darstellt.

-}

prettyTreeMarkedWith :: (a -> String) -> (a -> [a]) -> Int -> a -> String
prettyTreeMarkedWith label children stepNumber tree =
   let (treeText, _) = prettyTreeMarkedRecWith label children stepNumber tree 1
   in treeText

{- 

Diese Funktion soll einen Ausdrucksbaum als einen String zurückgeben, bei dem der aktuelle Knoten markiert wird.

Sie bekommt als ersten Parameter die aktuelle Schrittnummer.
Diese Schrittnummer kommt aus der Traversierung, also z.B. Schritt 1, Schritt 2, Schritt 3 usw.

Als zweiten Parameter bekommt sie den Ausdrucksbaum, der angezeigt werden soll.

Die Nummerierung läuft hier in Preorder-Reihenfolge.
Das bedeutet: zuerst der aktuelle Knoten, dann der linke Teilbaum, dann der rechte Teilbaum.

Wenn die aktuelle Schrittnummer z.B. 3 ist, wird der dritte Knoten im Baum mit >> << markiert.

-}

prettyTreeMarkedRecWith :: (a -> String) -> (a -> [a]) -> Int -> a -> Int -> (String, Int)
prettyTreeMarkedRecWith label children stepNumber tree currentNumber =
   let markedLabel = markedTreeLabel stepNumber currentNumber (label tree)
       childTrees = children tree
       (childrenText, nextNumber) = prettyChildrenMarkedWith label children stepNumber (currentNumber + 1) "" childTrees
   in (markedLabel ++ "\n" ++ childrenText, nextNumber)

{- Diese Hilfsfunktion markiert den Knoten, wenn seine Nummer der Schrittnummer entspricht. -}
markedTreeLabel :: Int -> Int -> String -> String
markedTreeLabel stepNumber currentNumber label
   | stepNumber == currentNumber = ">> " ++ label ++ " <<"
   | otherwise = label

{-

Diese Hilfsfunktion formatiert die Kinder eines Knotens, wenn sie markiert werden sollen. 

Wenn die Liste der Kindknoten leer ist, wird ein leerer String zurückgegeben.
Wenn die Liste der Kindknoten genau ein Element enthält, wird dieses Element mit einem "`-- " Präfix dargestellt.
Wenn die Liste der Kindknoten mindestens (1 oder mehr) ein Element enthält, wird das erste Element mit einem "|-- " Präfix dargestellt und 
die restlichen Elemente werden rekursiv mit einem "|   " Präfix dargestellt.

-}

prettyChildrenMarkedWith :: (a -> String) -> (a -> [a]) -> Int -> Int -> String -> [a] -> (String, Int)
prettyChildrenMarkedWith _ _ _ currentNumber _ [] = ("", currentNumber)
prettyChildrenMarkedWith label children stepNumber currentNumber prefix [child] =
   let (childText, nextNumber) = prettyTreeMarkedRecWith label children stepNumber child currentNumber
       formattedChild = formatMarkedChild prefix "`-- " "    " childText
   in (formattedChild, nextNumber)
prettyChildrenMarkedWith label children stepNumber currentNumber prefix (child:rest) =
   let (childText, nextNumberAfterChild) = prettyTreeMarkedRecWith label children stepNumber child currentNumber
       formattedChild = formatMarkedChild prefix "|-- " "|   " childText
       (restText, nextNumberAfterRest) = prettyChildrenMarkedWith label children stepNumber nextNumberAfterChild prefix rest
   in (formattedChild ++ restText, nextNumberAfterRest)

{- 

Diese Hilfsfunktion formatiert einen markierten Kindknoten. 
Sie bekommt als Eingabe 4 Strings: prefix, firstPrefix, restPrefix und childText.

prefix ist der Präfix, der vor dem Kindknoten steht, z.B. "|   ".
firstPrefix ist der Präfix, der vor dem ersten Kindknoten steht, z.B. "`-- ".
restPrefix ist der Präfix, der vor den restlichen Kindknoten steht, z.B. "|   ".
childText ist der Text des Kindknotens, der formatiert werden soll.

Wenn die Liste der Kindknoten leer ist, wird ein leerer String zurückgegeben.
Wenn die Liste der Kindknoten mindestens ein Element enthält, wird das erste Element mit firstPrefix 
dargestellt und die restlichen Elemente werden rekursiv mit restPrefix dargestellt.

-}

formatMarkedChild :: String -> String -> String -> String -> String
formatMarkedChild prefix firstPrefix restPrefix childText =
   case lines childText of
      [] -> ""
      (firstLine:restLines) ->
         prefix ++ firstPrefix ++ firstLine ++ "\n"
         ++ formatMarkedChildRest prefix restPrefix restLines

{-

Diese Hilfsfunktion formatiert den Rest eines markierten Kindknotens. 
Sie bekommt als Eingabe 3 Strings: prefix, restPrefix und childLines.

prefix ist der Präfix, der vor dem Kindknoten steht, z.B. "|   ".
restPrefix ist der Präfix, der vor den restlichen Kindknoten steht, z.B. "|   ".
childLines ist die Liste der Zeilen des Kindknotens, die formatiert werden sollen.

Wenn die Liste der Kindknoten leer ist, wird ein leerer String zurückgegeben.
Wenn die Liste der Kindknoten mindestens ein Element enthält, wird das erste Element mit 
restPrefix dargestellt und die restlichen Elemente werden rekursiv mit restPrefix dargestellt.

-}

formatMarkedChildRest :: String -> String -> [String] -> String
formatMarkedChildRest _ _ [] = ""
formatMarkedChildRest prefix restPrefix (line:linesRest) =
   prefix ++ restPrefix ++ line ++ "\n" ++ formatMarkedChildRest prefix restPrefix linesRest

{- 

Diese Funktion dient dazu, eine Liste von Animationsschritten in einen String umzuwandeln, 
der die einzelnen Schritte in einer lesbaren Form darstellt.

Sie bekommt die Liste der besuchten Knoten übergeben und ruft die rekursive Hilfsfunktion formatVisitedStepsRec auf, 
die die Liste der besuchten Knoten durchgeht und einen String baut, beginnend bei Schritt 1.

Es erzeugt eine Bsp-Ausgabe: formatVisitedSteps ["A", "B", "C"] -> "Schritt: 1, Knoten: A\nSchritt: 2, Knoten: B\nSchritt: 3, Knoten: C\n"

-}

formatVisitedSteps :: [String] -> String
formatVisitedSteps xs = formatVisitedStepsRec xs 1

{- 

Diese rekursive Hilfsfunktion geht die Liste der besuchten Knoten durch und baut einen String, der die einzelnen Schritte in einer lesbaren Form darstellt.
Sie bekommt die Liste der besuchten Knoten und die aktuelle Schrittnummer übergeben.

-}

formatVisitedStepsRec :: [String] -> Int -> String
formatVisitedStepsRec [] _ = ""
formatVisitedStepsRec (x:xs) stepnumber = "Schritt: " ++ show stepnumber ++ ", Knoten: " ++ x ++ "\n" ++ formatVisitedStepsRec xs (stepnumber + 1)

{- Traversalformate -}

{- 

Diese Funktion dient dazu, eine Liste von Traversierungsschritten in einen String umzuwandeln, 
der die einzelnen Schritte in einer lesbaren Form darstellt.

-}

showTraversal :: [String] -> String
showTraversal xs = unwords xs
