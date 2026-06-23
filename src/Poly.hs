
module Poly where

import Data.Ratio
import Data.List
import qualified Control.Applicative as Liste

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


{-

normalize bringt ein Polynom in eine eindeutige Normalform.

Dazu wird die Liste der Monome zuerst einmal nach den Exponenten
absteigend sortiert.

Die Liste der Monome wird dabei an sortBy übergeben.

sortBy ist eine Funktion aus Data.List und ermöglicht es, Listen
nach einer selbst definierten Vergleichsregel zu sortieren.

Die Vergleichsregel ist hier compareExponent.

Wichtig ist dabei: compareExponent bekommt nicht die ganze Liste übergeben. Die ganze Liste bekommt sortBy.

sortBy nimmt sich beim Sortieren immer zwei einzelne Monome aus
der Liste und ruft damit compareExponent auf.

Also zum Beispiel: compareExponent (M 3 2) (M 5 4)

Dadurch weiß sortBy, welches der beiden Monome weiter vorne
stehen soll.

Hierzu vergleicht compareExponent zwei Monome ausschließlich
anhand ihrer Exponenten.

Da nach dem Sortieren Monome mit gleichem Exponenten direkt
nebeneinander stehen, können diese danach einfach zusammengefasst
werden.

Das eigentliche Zusammenfassen und Entfernen von 0-Koeffizienten
übernimmt die Hilfsfunktion combine.

Dadurch wird normalize nicht unnötig oft aufgerufen.

Beispiel:

[M 3 2, M 5 5, M 2 1] wird zuerst zu
[M 5 5, M 3 2, M 2 1]

Falls gleiche Exponenten vorkommen, stehen diese nach dem Sortieren
direkt nebeneinander.

Beispiel:

[M 2 2, M 5 5, M 3 2] wird zuerst zu
[M 5 5, M 2 2, M 3 2]

Danach kann combine die beiden Monome mit Exponent 2 zu einem
Monom zusammenfassen.

Am Ende ist garantiert:

- keine Monome mit Koeffizient 0 vorhanden
- gleiche Exponenten wurden zusammengefasst
- die Monome sind nach Exponenten absteigend sortiert

Das Ergebnis ist somit immer ein vollständig normalisiertes Polynom.

-}

normalize :: Poly -> Poly
normalize (P xs) =
    P (combine (sortBy compareExponent xs))

{-

combine fasst eine bereits sortierte Liste von Monomen zusammen.

Wichtig ist: combine geht davon aus, dass die Liste schon nach Exponenten absteigend sortiert wurde.

Deshalb müssen gleiche Exponenten nicht mehr gesucht werden,
sondern stehen direkt nebeneinander.

Falls die Liste leer ist, wird einfach eine leere Liste zurückgegeben.

Fall: combine [] ergibt []

Falls die Liste nur aus einem Monom besteht, wird geprüft, ob der
Koeffizient 0 ist.

Ist der Koeffizient 0, wird das Monom entfernt, da 0*x^e immer 0
ergibt.

Beispiel: combine [M 0 3] ergibt []

Ist der Koeffizient nicht 0, bleibt das Monom erhalten.

Beispiel: combine [M 5 3] ergibt [M 5 3]

Falls die Liste aus mindestens zwei Monomen besteht, werden immer
die ersten beiden Monome betrachtet.

Zuerst wird geprüft, ob der Koeffizient des ersten Monoms 0 ist.

Ist das der Fall, wird dieses Monom entfernt und combine wird mit
dem zweiten Monom und dem Rest der Liste fortgesetzt.

Beispiel: combine [M 0 4, M 3 2] ergibt dasselbe wie combine [M 3 2]

Danach wird geprüft, ob der Koeffizient des zweiten Monoms 0 ist.

Ist das der Fall, wird dieses Monom ebenfalls entfernt und combine
wird mit dem ersten Monom und dem Rest der Liste fortgesetzt.

Beispiel: combine [M 5 4, M 0 2] ergibt dasselbe wie combine [M 5 4]

Als Nächstes wird geprüft, ob beide Monome den gleichen Exponenten
besitzen.

Falls ja, werden beide Koeffizienten addiert und zu einem Monom
zusammengefasst.

Beispiel: 2x² + 5x² = 7x² , also: combine [M 2 2, M 5 2] ergibt [M 7 2]

Es kann jedoch passieren, dass sich beide Koeffizienten gegenseitig
aufheben.

Beispiel: 2x² + (-2x²) = 0 , also: combine [M 2 2, M (-2) 2] ergibt []

In diesem Fall werden beide Monome entfernt und das Zusammenfassen
wird mit dem Rest der Liste fortgesetzt.

Falls die Exponenten verschieden sind, bleibt das erste Monom
erhalten.

Der Grund dafür ist, dass die Liste bereits sortiert ist.

Wenn also der Exponent des ersten Monoms verschieden vom Exponenten
des zweiten Monoms ist, kann später kein weiteres Monom mit diesem
Exponent kommen.

Deshalb kann das erste Monom direkt in das Ergebnis übernommen
werden.

Danach wird nur der Rest der Liste weiter zusammengefasst.

Beispiel: combine [M 5 4, M 3 2, M 1 1] ergibt M 5 4 : combine [M 3 2, M 1 1]

Da die Liste vorher bereits sortiert wurde, muss combine nicht
nochmal sortieren.

-}

combine :: [Monom] -> [Monom]
combine [] = []
combine [M k e]
    | k == 0    = []
    | otherwise = [M k e]
combine ((M k1 e1):(M k2 e2):xs)
    | k1 == 0 =
        combine ((M k2 e2):xs)

    | k2 == 0 =
        combine ((M k1 e1):xs)

    | e1 == e2 =
        let k = k1 + k2
        in if k == 0
           then combine xs
           else combine ((M k e1):xs)

    | otherwise =
        (M k1 e1) : combine ((M k2 e2):xs)


{-

compareExponent wird von sortBy verwendet, um zwei einzelne Monome
anhand ihrer Exponenten zu vergleichen.

compareExponent bekommt also nicht die ganze Monomliste.

Die ganze Liste bekommt sortBy.

sortBy ruft compareExponent intern immer wieder mit zwei Monomen
aus der Liste auf.

Die Funktion compare liefert einen Wert vom Typ Ordering zurück.
Dieser Datentyp besitzt die drei möglichen Werte:

LT -> kleiner als
EQ -> gleich
GT -> größer als

Da hier compare e2 e1 und nicht compare e1 e2 verwendet wird,
erfolgt die Sortierung in absteigender Reihenfolge der Exponenten.

Beispiel:
compareExponent (M 3 2) (M 5 4) entspricht dem Vergleich compare 4 2

und liefert GT zurück. Dadurch wird das Monom mit Exponent 4
vor dem Monom mit Exponent 2 einsortiert.

-}

compareExponent :: Monom -> Monom -> Ordering
compareExponent (M k1 e1) (M k2 e2) =
    compare e2 e1

{-

Diese Funktion soll ein Polynom in ein negatives Polynom verwandeln.

Das unäre "-" wird in Haskell von der Typklasse Num als negate interpretiert. Wenn ich hier direkt -(P xs) oder negate (P xs) schreiben würde, ruft Haskell intern negate (P xs) auf. Unten wurde jedoch definiert:
negate p = negat p, dadurch würde negate wieder negat aufrufen und negat wieder negate, wodurch eine Endlosschleife entstehen würde. Deshalb wird der Koeffizient jedes Monoms direkt mit (-k) negiert.

Zum Beispiel macht negat (P [M 2 2, M 2 1])
mathematisch aus 2x² + 2x das Polynom -(2x² + 2x) bzw. -2x² - 2x.

Als Erstes wird die Hilfsfunktion negatRec aufgerufen. Diese läuft rekursiv durch alle Monome und negiert nur die Koeffizienten. Erst nachdem alle Monome bearbeitet wurden, wird einmal normalize auf das gesamte Ergebnis angewendet.
Dadurch ist sichergestellt, dass auch dann ein normalisiertes Polynom zurückgegeben wird, wenn ein nicht normalisiertes Polynom als Parameter übergeben wurde. Gleichzeitig wird normalize nur einmal am Ende ausgeführt und nicht bei jedem Rekursionsschritt, wodurch die Laufzeit verbessert wird.

Wenn ein leeres Polynom als Argument übergeben wird, dann wird ein leeres Polynom zurückgegeben.

Wenn ein Polynom mit genau einem Monom übergeben wird, wird ganz einfach das Polynom mit dem Monom zurückgegeben, der Koeffizient ist hierbei aber negiert.

Wenn ein Polynom mit mehr als einem Monom übergeben wird, erstellen wir einen Auspacker

let P rest = negatRec (P xs).

Wie bereits zuvor entsteht rechts ein Poly der Form P [Monom]. rest übernimmt dabei automatisch den inneren Teil [Monom] und kann deshalb hinter

(M (-k) e) :`

eingesetzt werden.

Mit dem let wird also festgelegt, dass die Restliste, also alle Monome außer dem ersten, rekursiv weiter durchlaufen und negiert werden sollen.
Anschließend wird das erste Monom mit negiertem Koeffizienten vorne angefügt und die bereits negierte Restliste dahinter gesetzt.
Nachdem negatRec alle Monome bearbeitet hat, wird das komplette Ergebnis noch einmal durch `normalize` normalisiert, sodass am Ende immer ein vollständig negiertes und normalisiertes Polynom entsteht.

Wir definieren die Hilfsfunktion nicht mit where, da wir negatRec brauchen um ein Polynom zu negieren, aber noch ohne, dass wir normalize aufrufen, denn bei sub, rufen wir mit add schon normalize auf.
Würden wir negat negat verwenden, die negatRec als Hilfsfunktion mit where implementiert hat, würden wir unnöritg ein 2-mal normalize aufrufen, deswegen besser die Negierung an sich trennen und die Normalisierung auf die Negierung.

-}

negat :: Poly -> Poly
negat (P xs) = normalize (negatRec (P xs))

negatRec :: Poly -> Poly
negatRec (P []) = P []
negatRec (P [M k e]) = P [M (-k) e]
negatRec (P ((M k e) : xs)) = let P rest = negatRec (P xs) in P ((M (-k) e):rest)


{-

Diese Funktion soll eine Subtraktion zweier Polynome durchführen-

Hier können wir einfach die Funktion add benutezn, um zwei Polynome zu addieren, es gilt aber:
Das 2te Polynom muss negiert sein, damit es quasi als ein -(P xs2) interpretiert wird. Es wird erfüllt: 

P xs1 + (- P xs2) = P xs1 - P xs2

Mathematisches Bsp: 2x + 2 + (- 6x + 1) = 2x + 2 - 6x + 1

-}

sub :: Poly -> Poly -> Poly
sub (P xs1) (P xs2) = add (P xs1) (negatRec (P xs2))

{-

Diese Funktion soll zwei Polynome miteinander multiplizieren.

Die Polynome werden zuerst ausgepackt in der Form (P xs1) und (P xs2), 
denn wenn wir beispielsweise P [M 2 1, M 3 0] haben ist xs1 = [M 2 1, M 3 0]

Wir wenden List-Comprehension an. Es wird jedes Monom durchgangen aus dem ersten Polynom bei dem die Koeffizienten ungleich 0 sind, der Name von jedem i-ten Monom in dem
ersten Polynom ist M k1 e1. Dasselbe für den zweiten Polynom mit M k2 e2.

Es entsteht ein neues Polynom, wobei bei dem i-ten Monom der Koeffizient wie folgt entsteht: Koeffizient des aktuellen Monoms von 
dem ersten Polynom multipliziert mit dem Koeffizienten des aktuellen Monoms der zweiten Liste.

Für den i-ten Exponenten des i-ten Monoms des neuen Polynoms gilt folgendes: i-ter Exponent des i-ten Monoms von dem ersten Polynom addiert
mit dem i-ten Exponenten des i-ten Monoms von dem zweiten Polynom.

Das Ergebnis wird logischerweise normalisiert

-}

mult :: Poly -> Poly -> Poly
mult (P xs1) (P xs2) = normalize (P [ M (k1 * k2) (e1 + e2) | M k1 e1 <- xs1, k1 /= 0, M k2 e2 <- xs2, k2 /= 0])

{-

Diese Funktion berechnet die Ableitung eines Polynoms.

Wir durchlaufen die Monomliste des Polynoms und erstellen ein i-tes, bzw. aktuelles, Monom. Es werden nur die Exponenten, welche
Größer als 0 sind betrachtet, da bei der Ableitung des Monoms mit einem 0-Exponenten, der Monom verschwindet, so wie wir es aus der Mathematik kennen.

Es wird ein neues Polynom erstellt, für den i-ten Koeffizienten des i-ten Monoms von dem neuen Polynom gilt: Koeffizient * Exponent, der Exponent wird in einen beliebigen Datentypen angepasst,
damit die Multiplikation von den Datentypen her funktioniert.

Für den i-ten Exponenten des i-ten Monoms von dem neuen Polynom gilt: Der Exponent verringert sich um Eins, so wie wir es aus der Mathematik kennen.

-}

derivation :: Poly -> Poly
derivation (P xs) = normalize (P [ M (k * fromIntegral e) (e - 1) | M k e <- xs, e > 0 ])