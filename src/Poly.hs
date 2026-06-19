
module Poly where

import Data.Ratio
import Data.List

{-
Ein Monom ist ein einzelner Term, z.b 3x^2 oder 5x
Das Rational ist hier der Koeffizient (z.B 3 oder 5) und das Int der Exponent (also z.b ^2)
Bei dem Rational ist hier das Einsetzen einer Ganzzahl auch möglich, Haskell interpretiert das automatisch.
Ein Bruch wäre z.B 3 % 5 = 3/5 tel 

deriving (Show, Eq) erzeugt automatisch Standardfunktionen.
Show: Damit kann Haskell den Datentyp als Text anzeigen. Beispiel: M 3 2 wird in GHCi angezeigt als: M 3 2
Ohne Show könntest du den Wert nicht einfach ausgeben.
Eq: Damit kann man vergleichen: M 3 2 == M 3 2 ergibt: True, Ohne Eq gäbe es keinen ==-Vergleich.
M ist der Konstruktor, mit welchem man sagt, dass es sich um ein Monom handelt.

data kann mehrere Konstruktoren, auch Parameter, haben und mehrere Werte speichern. Rational und Int sind Parameter

-}

data Monom = M Rational Int
 deriving (Show,Eq)



{-
Der neu definierte Datentyp Poly ist eine Liste von Monomen, also z.b im Code wäre es als Bsp sowas:
P [M 2 3, M 3 4, M 2 1], mathematisch würde es so aussehen: [2x^3, 3x^4,2x^1]
P ist der Konstruktor, was einen richtiges Poly erzeugt (siehe eine Zeile davor Bsp.). Ohne das P, also nur [M 2 3, M 3 4, M 2 1]
wäre es lediglich eine Monomliste, aber mit P sagen wir nochmal, dass diese Liste als Polynom behandelt werden soll

newtype kann nur einen Konstruktor, auch nur einen Parameter haben, und auch genau nur einen Wert speichern.
-}

newtype Poly = P [Monom] 
 deriving (Show,Eq)

{-

Infix ermöglicht es, anstatt der Schreibweise (#^) 3 2, die Schreibweise 3 #^ 2 anzuwenden.
Dieser Infixoperator erstellt ein Polynom mit genau einem Monom

-}

infix 8 #^ 
(#^) :: Rational -> Int -> Poly
a #^  b = P [M a b] 


{-

Bsp Anwendung der Infixoperation: 3#^2 macht ganz einfach P [M 3 2]

-}

polyEx :: Poly 
polyEx = 3#^2

{-

Evaluate ist die Funktion, welche die Auswertund durchführt.
Der Infixoperator § steht für die Auswertung da.

Hier wird List Comprehension angewendet, welche die allgemeine Notation [Ausdruck | Generator]
besitzt. z.B bedeuted [x * 2 | x <- [1,2,3]] dass x jeden Wert aus der Liste [1,2,3] animmt und somit 
dass [1*2,2*2,3*2] = [2,4,6] rauskommt.

M k e bedeuted k*x^e

(P xs) ist das Liste die als Polynom interpretiert wird und x ist der Wert, welcher in der Formel eingesetzt wird.

Für jedes Monom M k e aus der Liste xs wird k*(x^e) berechnet. Anschließend werden alle Ergebnisse mit sum addiert.

z.B rechnet evaluate (P [M 3 2, M 2 1, M 1 0]) 3
3·3² + 2·3 + 1 = 3·9 + 6 + 1 = 27 + 6 + 1 = 34 aus

-}

evaluate :: Poly -> Rational -> Rational
evaluate (P xs) x = sum [k * (x ^ e) | M k e <- xs]

infix 9 §
(§) :: Poly -> Rational -> Rational 
(§) = evaluate
 

{-
normalize bringt ein Polynom in eine eindeutige Normalform.

Falls das Polynom leer ist, wird einfach ein leeres Polynom
zurückgegeben.

Falls das Polynom nur aus einem Monom besteht, ist dieses bereits
normalisiert und wird unverändert zurückgegeben.

Ansonsten werden immer die ersten beiden Monome betrachtet.

Zuerst wird geprüft, ob der Koeffizient des ersten Monoms 0 ist.
Ist das der Fall, wird dieses Monom entfernt, da 0*x^e immer 0 ergibt.

Danach wird geprüft, ob der Koeffizient des zweiten Monoms 0 ist.
Ist das der Fall, wird dieses Monom ebenfalls entfernt.

Als Nächstes wird geprüft, ob beide Monome den gleichen Exponenten
besitzen. Falls ja, werden beide Koeffizienten addiert und zu einem
Monom zusammengefasst.

Beispiel:

2x² + 5x² = 7x²

Es kann jedoch passieren, dass sich beide Koeffizienten gegenseitig
aufheben.

Beispiel:

2x² + (-2x²) = 0

In diesem Fall werden beide Monome entfernt und die Normalisierung
wird mit dem Rest der Liste fortgesetzt.

Falls die Exponenten verschieden sind und bereits in der richtigen
Reihenfolge stehen (größter Exponent vorne), bleibt das erste Monom
erhalten und nur der Rest wird rekursiv normalisiert.

Falls die Monome nicht in der richtigen Reihenfolge stehen,
wird die komplette Liste zunächst mit sortBy nach den Exponenten
absteigend sortiert.

sortBy ist eine Funktion aus Data.List und ermöglicht es, Listen
nach einer selbst definierten Vergleichsregel zu sortieren.

Hierzu wird die Hilfsfunktion compareExponent verwendet.
Sie vergleicht zwei Monome ausschließlich anhand ihrer Exponenten.

Dadurch stehen Monome mit gleichem Exponenten nach dem Sortieren
automatisch nebeneinander und können in den nächsten
Rekursionsschritten einfach zusammengefasst werden.

Beispiel: [M 3 2, M 5 5, M 2 1] wird zu [M 5 5, M 3 2, M 2 1]
dadurch ist am Ende ist garantiert:

- keine Monome mit Koeffizient 0 vorhanden
- gleiche Exponenten wurden zusammengefasst
- die Monome sind nach Exponenten absteigend sortiert

Das Ergebnis ist somit immer ein vollständig normalisiertes Polynom.
-}

normalize :: Poly -> Poly
normalize (P []) = P []
normalize (P [m]) = P [m]
normalize (P ((M k1 e1):(M k2 e2):xs))
    | k1 == 0 =
        normalize (P ((M k2 e2):xs))

    | k2 == 0 =
        normalize (P ((M k1 e1):xs))

    | e1 == e2 =
        if k1 + k2 == 0
        then normalize (P xs)
        else normalize (P ((M (k1 + k2) e1):xs))

    | e1 > e2 =
        let P rest = normalize (P ((M k2 e2):xs))
        in P ((M k1 e1):rest)

    | otherwise =
        let sorted = sortBy compareExponent ((M k1 e1):(M k2 e2):xs)
        in normalize (P sorted)


{-
compareExponent wird von sortBy verwendet, um zwei Monome
anhand ihrer Exponenten zu vergleichen.

Die Funktion compare liefert einen Wert vom Typ Ordering zurück.
Dieser Datentyp besitzt die drei möglichen Werte:

LT -> kleiner als
EQ -> gleich
GT -> größer als

Da hier compare e2 e1 und nicht compare e1 e2 verwendet wird,
erfolgt die Sortierung in absteigender Reihenfolge der Exponenten.

Beispiel:

compareExponent (M 3 2) (M 5 4)

entspricht dem Vergleich

compare 4 2

und liefert GT zurück. Dadurch wird das Monom mit Exponent 4
vor dem Monom mit Exponent 2 einsortiert.
-}

compareExponent :: Monom -> Monom -> Ordering
compareExponent (M k1 e1) (M k2 e2) =
    compare e2 e1

{-

Diese Funktion soll ein Polynomaddition realisieren, sprich: (p1 + p2) (x) = p1(x) + p2(x)
Dabei soll das Ergebnispolynom normalisiert sein. 

Wir dürfen nicht einfach normalize (p1+p2) schreiben, da p1+p2 = add p1 p2 oben definiert ist.
Das würde eine Endlosschleife sein, da es sich immer selbst wieder aufrufen würde.

Wir können anstatt add p1 p2, lieber add (P xs1) (P xs2) schreiben, um an die Monomlisten ranzukommen.

mathmematisch geht es so, da: (2x² + 2x + 2) + (4x + 4) wird zuerst zu: 2x² + 2x + 2 + 4x + 4
Das ist mathematisch völlig korrekt. Erst DANACH vereinfacht man 2x + 4x = 6x zu 2 + 4 = 6 , Ergebnis: 2x² + 6x + 6

Das macht das Programm auch: Schritt 1 — Listen zusammenfügen: xs1 ++ xs2 macht: [M 2 2, M 2 1, M 2 0, M 4 1, M 4 0]
Das bedeutet mathematisch 2x² + 2x + 2 + 4x + 4, Schritt 2 — normalize:
Dann erkennt normalize die gleichen Exponenten, Also: 2x + 4x = 6x und 2 + 4 = 6

-}

add ::  Poly -> Poly -> Poly
add (P xs1) (P xs2) = normalize (P (xs1 ++ xs2))
