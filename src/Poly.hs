
module Poly where

import Data.Ratio
import Data.List
import qualified Control.Applicative as Liste
import qualified Control.Applicative as Polynoms
import qualified Control.Applicative as Polynom

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

Beispiel: 5*x^3 + 2*x wird zu: 15*x^2 + 2

-}

derivation :: Poly -> Poly
derivation (P xs) = normalize (P [ M (k * fromIntegral e) (e - 1) | M k e <- xs, e > 0 ])

{-

Diese Funktion führt eine Auswertung für ein Polynom durch.
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

Diese Funktion soll zwei Polynome dividieren, dabei gibt p1 /% p2 am Ende ein Paar zurück.

Der erste Wert in diesem Paar ist der Quotient und der zweite Wert ist
der Rest.

Zum Beispiel bedeutet p1 /% p2 = (q, r) mathematisch: p1 = q * p2 + r

Als Erstes werden beide Polynome normalisiert.

let dividend = normalize p1
    divisor  = normalize p2

dividend ist dabei das normalisierte Polynom, das geteilt werden soll.
divisor ist das normalisierte Polynom, durch das geteilt wird.

Das ist wichtig, weil die Polynomdivision immer mit dem größten
Exponenten vorne arbeitet. Wenn die Polynome nicht normalisiert wären,
könnte die Funktion das führende Monom nicht zuverlässig benutzen.

Danach wird geprüft, ob der Divisor das Nullpolynom ist.

Wenn normalize p2 also P [] ergibt, dann kann nicht dividiert werden,
weil man nicht durch 0 teilen darf.

Deshalb wird in diesem Fall ein Fehler ausgegeben: error "division by zero polynomial"

Falls der Divisor nicht leer ist, wird die eigentliche Division mit
divStep gestartet.

Dabei wird als dritter Parameter eine leere Liste [] übergeben.

Diese Liste sammelt nach und nach die Monome des Quotienten.

Also: divStep dividend divisor [] bedeutet: Starte die Polynomdivision mit dem normalisierten Dividend, dem
normalisierten Divisor und einem noch leeren Quotienten.

normalize wird hier nur am Anfang auf p1 und p2 angewendet.
Dadurch muss nicht bei jedem einzelnen Schritt wieder neu sortiert und
zusammengefasst werden.

-}

infix 2 /%
(/%) :: Poly -> Poly -> (Poly, Poly)
p1 /% p2 = let dividend = normalize p1
               divisor  = normalize p2
  in case divisor of
       P [] ->
         error "division by zero polynomial"

       P ds ->
         divStep dividend divisor []

{-

divStep führt die eigentliche Polynomdivision Schritt für Schritt aus.

Der erste Parameter ist der aktuelle Rest, also das Polynom, das noch
weiter geteilt werden soll.

Der zweite Parameter ist der Divisor. Dieser bleibt während der ganzen
Division gleich.

Der dritte Parameter ist qAcc. In dieser Liste werden die Monome des
Quotienten gesammelt.

Falls der aktuelle Rest leer ist, also P [], dann ist nichts mehr zu
teilen.

In diesem Fall wird der gesammelte Quotient normalisiert zurückgegeben
und der Rest ist P [].

Also: divStep (P []) divisor qAcc ergibt: (normalize (P qAcc), P [])

Falls der Divisor leer ist, wird wieder ein Fehler ausgegeben.
Eigentlich sollte dieser Fall nicht passieren, weil schon in /% geprüft
wurde, ob der Divisor P [] ist.

Trotzdem steht der Fall hier nochmal, damit divStep nicht mit einem
leeren Divisor weiterrechnet.

Wenn beide Polynome nicht leer sind, werden jeweils die ersten Monome
betrachtet.

Beim aktuellen Rest ist das: M kr er

kr ist der Koeffizient vom führenden Monom des aktuellen Restes.
er ist der Exponent vom führenden Monom des aktuellen Restes.

Beim Divisor ist das: M kd ed

kd ist der Koeffizient vom führenden Monom des Divisors.
ed ist der Exponent vom führenden Monom des Divisors.

Danach wird geprüft, ob er < ed gilt.

Wenn der größte Exponent vom aktuellen Rest kleiner ist als der größte
Exponent vom Divisor, kann nicht mehr weiter dividiert werden.

Zum Beispiel kann man 3x nicht weiter durch x² teilen, weil der
Exponent 1 kleiner ist als 2.

Dann ist qAcc der Quotient und der aktuelle Rest bleibt als Rest übrig.

Falls er >= ed gilt, kann ein weiterer Divisionsschritt gemacht werden.

Dazu wird zuerst das nächste Monom des Quotienten berechnet: qMonom = M (kr / kd) (er - ed)

Dabei werden die Koeffizienten geteilt und die Exponenten voneinander
abgezogen.

Zum Beispiel: 6x³ / 2x = 3x²

Danach wird dieses neue Quotienten-Monom mit dem Divisor multipliziert.

subtrahend = multMonom qMonom (P (M kd ed : ds))

Anschließend wird dieser subtrahend vom aktuellen Rest abgezogen.

Da es hier keine eigene direkte Subtraktion gibt, wird der subtrahend
erst mit negat negiert und dann mit add addiert.

nextRest = add (P (M kr er : rs)) (negat subtrahend)

Das entspricht mathematisch: aktueller Rest - subtrahend

Danach wird divStep rekursiv mit dem neuen Rest aufgerufen.
Das neue Quotienten-Monom wird vorne an qAcc angehängt.

So läuft die Division weiter, bis der Rest leer ist oder der Exponent
vom Rest kleiner als der Exponent vom Divisor ist.

-}

divStep :: Poly -> Poly -> [Monom] -> (Poly, Poly)
divStep (P []) divisor qAcc = (normalize (P qAcc), P [])
divStep rest (P []) qAcc = error "division by zero polynomial"
divStep (P (M kr er : rs)) (P (M kd ed : ds)) qAcc
  | er < ed = (normalize (P qAcc), normalize (P (M kr er : rs)))
  | otherwise =
      let qMonom     = M (kr / kd) (er - ed)
          subtrahend = multMonom qMonom (P (M kd ed : ds))
          nextRest   = add (P (M kr er : rs)) (negat subtrahend)
      in divStep nextRest (P (M kd ed : ds)) (qMonom : qAcc)

{-

multMonom multipliziert ein einzelnes Monom mit einem ganzen Polynom.

Diese Hilfsfunktion wird bei der Polynomdivision benutzt.

Man könnte hier auch die normale mult Funktion verwenden, also zum
Beispiel: mult (P [qMonom]) divisor, das würde mathematisch auch funktionieren.

Das Problem ist aber, dass mult am Ende wieder normalize aufruft.
Bei der Polynomdivision passiert dieser Schritt sehr oft, also würde
normalize dadurch unnötig oft ausgeführt werden, deshalb gibt es hier multMonom.

multMonom bekommt ein Monom M k e und ein Polynom P xs 

Wir durchlaufen die Monomliste aus dem übergebenem Monomparameter und erstellen davon immer ein i-tes Monom M k2 e2.
Wir bauen mit der List-Comprehension ein neues Polynom zusammen. 

Für jedes i-te Monom in dem neuen Polynom gilt für den i-ten Koeffizienten: Koeffizient aus dem übergebenen Parameter-Monom multipliziert mit 
dem i-ten Koeffizienten aus der Monomliste des Parameter-Polynoms.

Für jedes i-te Monom in dem neuen Polynom gilt für den i-ten Exponenten: Exponent aus dem Parameter-Monom wird addiert mit dem i-ten Exponenten aus 
der Monomliste des Parameter-Polynoms.

Da hier nur ein einzelnes Monom mit einem bereits normalisierten
Polynom multipliziert wird, muss nicht direkt in multMonom normalisiert
werden.

Die Normalisierung passiert später durch add oder am Ende beim
Quotienten.

Dadurch bleibt die Funktion einfacher und vermeidet unnötige
normalize-Aufrufe.

-}

multMonom :: Monom -> Poly -> Poly
multMonom (M k e) (P xs) =
  P [M (k * k2) (e + e2) | M k2 e2 <- xs]

{-

Diese Funktion soll ganze Polynome mit +, -, * und Zahlenliteralen funktionsfähig machen

Mit instance Num Poly where sagen wir, dass Poly zur Typklasse Num gehören soll, die Wirkung davon ist, dass ein Poly
als eine Zahl interpretiert wird und wir darauf die Standard-Zahlenoperationen verwenden können.

fromInteger 0 = P [] sagt aus, was passieren soll wenn Haskell die Zahl 0 als Polynom braucht. Es entsteht ein leeres Polynom. 
fromInteger x = P [M (fromInteger x % 1) 0] stellt die Schreibweise als Polynom dar, für beliebig andere Zahlen, z.B wenn für x die 5 eingesetzt wird,
dann wird ein Polynom in dieser Schreibweise zurückgegeben: P [M (5 % 1) 0]

Wenn wir (-p1= schreiben, intepretiert Haskell das unäre Minus als negate, und sobald wir negate p, also (-p) schreiben, wird unsere selsbtdefinierte
negat Funktion aufgerufen.

wenn wir ganze Polynome miteinander addieren wollen, also z.B (P [M 2 2, M 2 1]) + (P [M 4 2, M 4 1]) schreiben, wird durch die instance-Methode das + als unsere
selbstdefinierte add Funktion interpretiert, sprich es wird add (P [M 2 2, M 2 1]) (P [M 4 2, M 4 1]) aufgerufen.

Für die anderen Operationen ist dies der Gleiche Ablauf.

-}

instance Num Poly where

  fromInteger :: Integer -> Poly
  fromInteger 0 = P []
  fromInteger x = P [M (fromInteger x % 1) 0]

  negate :: Poly -> Poly
  negate p = negat p

  (+) :: Poly -> Poly -> Poly
  p1 + p2 = add p1 p2

  (-) :: Poly -> Poly -> Poly
  p1 - p2 = sub p1 p2 

  (*) :: Poly -> Poly -> Poly
  p1 * p2 = mult p1 p2

  abs :: Poly -> Poly
  abs _ = error "abs nicht implementiert für Polynome"

  signum :: Poly -> Poly
  signum _ = error "signum nicht implementiert für Polynome"


{-

Diese Klasse, welche als Typklasse verwenden werden kann, sagt, dass ein Datentyp in einen LaTeX-String
umgewandelt werden kann.

Wie genau dieser Wert umgewandelt wird, wird später in den einzelnen
Instanzen definiert.

-}

class ToLaTeX a where
  toLaTeX :: a -> String


{-

Diese Instanz beschreibt, wie ein Bruch vom Typ Ratio a als LaTeX ausgegeben wird.

Mit numerator r holen wir den Zähler aus dem Bruch.
Mit denominator r holen wir den Nenner aus dem Bruch.

Diese Werte speichern wir als z und n.

Falls der Nenner n gleich 1 ist, wird nur der Zähler ausgegeben, weil z/1 einfach z ist.

Falls der Nenner nicht 1 ist, wird ein LaTeX-Bruch erzeugt.

Zum Beispiel wird aus 3 % 5 der String "\frac{3}{5}".

Da der Backslash in Haskell eine besondere Bedeutung hat, schreiben
wir "\\frac", damit im Ergebnis \frac steht.

-}

instance (Integral a, Eq a, Num a, Show a) => ToLaTeX (Ratio a) where

  toLaTeX :: (Integral a, Eq a, Num a, Show a) => Ratio a -> String
  toLaTeX r
    | n == 1    = show z
    | otherwise = "\\frac{" ++ show z ++ "}{" ++ show n ++ "}"
    where
      z = numerator r
      n = denominator r


{-

Diese Instanz beschreibt, wie ein ganzes Polynom als LaTeX ausgegeben wird.

Das Polynom wird zuerst normalisiert.

Danach wird polyToLaTeX aufgerufen, welches die eigentliche Ausgabe als String übernimmt.

Bsp: toLaTeX (P [M 2 1, M 3 2, M 4 1]) toLaTeX (P [M 2 1, M 3 2, M 4 1]), 
zuerst passiert: normalize (P [M 2 1, M 3 2, M 4 1])
Das wird zu: P [M 3 2, M 6 1], weil mathmatisch  2x + 4x = 6x ergibt und nach Exponenten sortiert wird.
Danach macht wird aufgerufen: polyToLaTeX (P [M 3 2, M 6 1]) und als Ergebnis kommt "3*x^{2}+6*x"

-}

instance ToLaTeX Poly where

  toLaTeX :: Poly -> String
  toLaTeX p = polyToLaTeX (normalize p)



