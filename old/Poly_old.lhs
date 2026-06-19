







> module Poly where





> import Data.Ratio





> import Data.List
> import Type.Reflection.Unsafe (someTypeRepFingerprint)
> import qualified Control.Applicative as Liste
  



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

> data Monom = M Rational Int
>   deriving (Show,Eq)




Der neu definierte Datentyp Poly ist eine Liste von Monomen, also z.b im Code wäre es als Bsp sowas:
P [M 2 3, M 3 4, M 2 1], mathematisch würde es so aussehen: [2x^3, 3x^4,2x^1]
P ist der Konstruktor, was einen richtiges Poly erzeugt (siehe eine Zeile davor Bsp.). Ohne das P, also nur [M 2 3, M 3 4, M 2 1]
wäre es lediglich eine Monomliste, aber mit P sagen wir nochmal, dass diese Liste als Polynom behandelt werden soll

newtype kann nur einen Konstruktor, auch nur einen Parameter haben, und auch genau nur einen Wert speichern.

> newtype Poly = P [Monom] 
>   deriving (Show,Eq)





> infix 8 #^ 
> a #^  b = P [M a b] 





> polyEx = 3#^2





> class ToLaTeX a where
>   toLaTeX :: a -> String





> instance (Integral a,Eq a,Num a,Show a) => ToLaTeX (Ratio a)where
>   toLaTeX r
>    |n==1 = show z
>    |otherwise = "\\frac{"++show z++"}{"++show n++"}"
>     where
>       z = numerator r
>       n = denominator r





> instance ToLaTeX Monom where





>   toLaTeX (M k 0) = toLaTeX k





>   toLaTeX (M k 1)
>     |k==1 = "x"





>     |otherwise = toLaTeX k++"*x"




Hier wird definiert, dass M k e = k*x^e bedeuted

>   toLaTeX (M k e)
>     |k==1 = "x^{"++show e++"}"
>     |otherwise = toLaTeX k++"*x^{"++show e++"}"





> instance ToLaTeX Poly where
>   toLaTeX (P []) = "0"
>   toLaTeX (P (x:xs))
>     = (toLaTeX x)
>        ++ (foldl  (++)  ""
>            $ map (\m@(M k e)->
>                       (if (k>=0) then "+" else "")++toLaTeX m)
>              xs)



Hier wird List Comprehension angewendet, welche die allgemeine Notation [Ausdruck | Generator]
besitzt. z.B bedeuted [x * 2 | x <- [1,2,3]] dass x jeden Wert aus der Liste [1,2,3] animmt und somit 
dass [1*2,2*2,3*2] = [2,4,6] rauskommt.

M k e bedeuted k*x^e

(P xs) ist das Liste die als Polynom interpretiert wird und x ist der Wert, welcher in der Formel eingesetzt wird.

Für jedes Monom M k e aus der Liste xs wird k*(x^e) berechnet. Anschließend werden alle Ergebnisse mit sum addiert.

> infix 9 §
> (§) :: Poly -> Rational -> Rational
> (P xs)§x = sum [k * (x ^ e) | M k e <- xs]



Anstatt P (xs) kann man auch P (x:y:xs) schreiben, x ist das Erste Monom in der Liste und y das Zweite. 
Da man einzelne Monoms auch als M Rational Int M k e , bzw. (M k e), schreiben kann, geht auch P ((M k1 e1):(M k2 e2):xs)

Falls ein Polynom mit keinem Monom angegeben wird, wird ein leeres Polynom zurückgegeben.
Falls ein Polynom mit genau einem Monom angegeben wird, wird das Polynom mit dem Monom zurück gegeben, zb normalize (P [M 2 3]) = wird einfach 2x^3 zurückgegeben.

Ansonsten (Wenn ein Polynom mit mehreren Monomen als Parameter eingegeben wird):
Als Erstes wird geprüft ob der Erste Monom den Koeffizient 0 hat, wenn ja fließt der erste Monom nicht in die Berechnung ein und es wird mit dem zweiten Monom 
und dem Rest rekursiv weiter gerechnet, da ein Monom mit Koeffizient 0 immer = 0 ist.

z.B 0x^2 + 2x + 2 = 2x + 2
Als Zweites wird geprüft ob der Zweite Monom den Koeffizient 0 hat, wenn ja dann fließt der zweite Monom nicht in die Berechnung ein und es wird mit dem ersten Monom 
und dem Rest außer dem zweiten Monom rekursiv weiter gerechnet.
z.B 0x^2 + 2x + 2 = 2x + 2

Als Drittes wird geprüft ob der Exponent von dem ersten Monom gleich ist wie der Exponent vom zweiten Monom (es werden 2er Nachbar Paare verglichen),
wenn ja, dann wird der werden die beiden Monome zu einem Monom zusammengefasst, in dem die Koeffizienten beider addiert werden. Es wird mit dem zusammengefasstem Monom und 
dem Rest rekursiv weiter gerechnet. z.B 2x^2 + 2x + 4x + 2 = 2x^2 + 6x + 2

Als Viertes Prüfen wir die Ordnung der Exponenten, der Größte soll ganz Vorne sein und ab da Absteigen bis zum Niedrigsten Exponenten.
Wenn der Exponent des ersten Monums kleiner ist, als der Exponent des ZWeiten Monoms, dann werden die Monome ganz einfach getauscht und es wird rekursiv die Liste weiter geprüft. 

Ansonsten, wenn keine dieser Prüfungen auf das Polynom trifft, also die ersten zwei Monome okay sind, muss ja das nächste geprüft werden, also das zweite mit dem 3 um sicherzustellen, 
dass der dritte Monom und so okay ist, und dann der Dritte mit dem Vierten und immer so weiter. Es muss der erste Monom (in der aktuellen Rekursionsebene) behalten werden, da eer sonst verloren geht und wir normalisieren den Rest weiter, 
damit wir am Ende einen komplett normalisiertes Polynom haben.
Beispiel:
[M 5 5, M 2 3, M 4 3]
Die ersten zwei Monome sind okay, die Prüfungen der Ifs nicht auf Sie zutreffen.
Deshalb bleibt M 5 5 vorne erhalten.
Der Rest [M 2 3, M 4 3] wird weiter normalisiert.
Daraus wird [M 6 3], weil die Exponenten gleich sind.
Danach wird das erste Monom wieder vorne angehängt: [M 5 5, M 6 3]

Es wird let P rest geschrieben, da wir rechts von (M k2 e2): ... eine Liste erwartet wird und wenn wir sagen P rest = (irgend ein Polynom) und Poly hat die form P [Monom],
dann ist rest = [Monom] , anderer Vergleich: data Person = Person String Int, wir machen: Person "Max" 22, dann auspacken: let Person name age = Person "Max" 22
Dann: name = "Max" age = 22, Genau dasselbe bei dir: let P rest = P [M 2 3, M 4 1] mit rest = [M 2 3, M 4 1], dann hat rest das [Monom] Parameter übernommen und ist eine Liste, daher können wir es einsetzen.
aus let P rest = normalize (P ((M k2 e2):xs)) entsteht ein Poly, also P [Monom] und rest übernimmt dann [Monom].
Wenn wir let rest = normalize (P ((M k2 e2):xs)), dann wäre rest einfach ein Poly und wir könnten es so nicht einsetzen

Es gibt aber ein Problem, welches entstehen kann, wenn es dazu kommt, dass wir gleiche Exponenten haben und diese Zahl zusammenfassen müssen. Da ein Rational auch eine negative Zahl sein kann,
wie zum Beispiel (-2) % 1 = -2, kann es sein, dass sich durch Addierung dieser Koeffzienten sich die aufheben, Veranschaulichung: M 2 2 + M (-2) 2 = 2x^2 + (-2x^2) = 0, also heben Sie sich dadurch auf.
Deshalb müssen wir noch zwei ifs unter dieser Prüfung tun. Das verschaltete If prüft, ob Monom 1 und Monom 2 sich wirklich aufheben, wenn ja werden diese Beiden nicht mehr betrachtet und es wird weiter mit den Restmonomen fortgefahren.
Das verschachtelte else macht dann ganz normal die Zusammenfassung der Zwei Koffezienten von Monom 1 und Monom 2 zu einem Monomen, wie schon oben beschrieben.

> normalize :: Poly -> Poly
> normalize (P []) = P []
> normalize (P [m]) = P [m]
> normalize (P ((M k1 e1):(M k2 e2):xs)) = if k1 == 0 then normalize (P ((M k2 e2):xs))
>                                          else if k2 == 0 then normalize (P ((M k1 e1):xs)) 
>                                          else if (e1 == e2) then
>                                           if k1 + k2 == 0 then normalize (P xs)
>                                           else normalize (P ((M (k1+k2) e1):xs)) 
>                                          else if (e1 < e2) then normalize (P ((M k2 e2):(M k1 e1):xs))
>                                          else let P rest = normalize (P ((M k2 e2):xs))
>                                               in P ((M k1 e1):rest)    
                                             



Diese Funktion soll ein Polynomaddition realisieren, sprich: (p1 + p2) (x) = p1(x) + p2(x)
Dabei soll das Ergebnispolynom normalisiert sein. Wir dürfen nicht einfach normalize (p1+p2) schreiben, da p1+p2 = add p1 p2 oben definiert ist.
Das würde eine Endlosschleife sein, da es sich immer selbst wieder aufrufen würde.
Wir können anstatt add p1 p2, lieber add (P xs1) (P xs2) schreiben, um an die Monomlisten ranzukommen.
mathmematisch geht es so, da: (2x² + 2x + 2) + (4x + 4) wird zuerst zu: 2x² + 2x + 2 + 4x + 4
Das ist mathematisch völlig korrekt. Erst DANACH vereinfacht man 2x + 4x = 6x zu 2 + 4 = 6 , Ergebnis: 2x² + 6x + 6
Das macht das Programm auch: Schritt 1 — Listen zusammenfügen: xs1 ++ xs2 macht: [M 2 2, M 2 1, M 2 0, M 4 1, M 4 0]
Das bedeutet mathematisch 2x² + 2x + 2 + 4x + 4, Schritt 2 — normalize:
Dann erkennt normalize die gleichen Exponenten, Also: 2x + 4x = 6x und 2 + 4 = 6

> add ::  Poly -> Poly -> Poly
> add (P xs1) (P xs2) = normalize (P (xs1 ++ xs2))


Das unäre "-" wird in Haskell zu von der Klasse num als negate interpretiert, und wenn ich hier -(P xs) schreibe,
ruft Haskell negate (P xs) auf und es wurde unten in Zeile 259 definiert: negate p = negat p, also würde eine Endlosschleife entstehen, 
wenn ich direkt negate oder "-" verwenden würde.

Diese FUnktion soll ein Polynom in ein negatives Polynom verwandeln,
z.B macht negat (P [M 2 2, M 2 1]) = - [M 2 2, M 2 1], also mathematisch 2x^2 + 2x zu -(2x^2 + 2x)

Wenn ein leerey Polynom als Argument übergeben wird, dann wird ein leeres Polynom zurückgeben.
Wenn ein Polynom mit einem Monom übergeben wird, wird ganz einfach das Polynom mit dem Monom zurückgegeben, der Koeffizient ist hierbei aber negiert! z.B bei negat (P [M 5 5]) = P [M (-5) 5]
mathematisch sind es so aus: 2x^2 = -2x^2

Wenn ein Polynomen mit mehr als ein Monom übergeben wird, erstellen wir einen Auspacker Poly let P rest = negat (normalize (P (xs))), wobei hier wie vorher ein P [Monom] entsteht
und rest den [Monom] übernimmt, daher können wir es hinter (M (-k) e): ... einsetzen, es wird mit dem let definiert, dass die Restliste, also die restlichen Monome außer dem Ersten, durchgegagen
werden sollen und negiert werden sollen. Es wird eingesetzt wo (in) das erste Monom negiert wird und dann die restliste mit dem definierten let durchgegangen wird. Es entsteht ein komplett negiertes Poly

> negat ::  Poly -> Poly
> negat (P []) = P []
> negat (P [M k e]) = P [M (-k) e]
> negat (P ((M k e) : xs)) = let P rest = negat (normalize (P (xs))) in P ((M (-k) e):rest)



> -- ====================================================================
> -- Aufgabe 1e: Multiplikation von zwei Polynomen
> -- ====================================================================
> mult :: Poly -> Poly -> Poly
> mult (P ps1) (P ps2) =
>   normalize (P [ M (k1 * k2) (e1 + e2) | M k1 e1 <- ps1, M k2 e2 <- ps2 ])

-- Erklärung: Setzt das mathematische Distributivgesetz ("Jedes Glied mal jedes Glied") um.
-- Die Formel zieht jedes Monom aus Liste 1 (M k1 e1) und kombiniert es mit jedem Monom
-- aus Liste 2 (M k2 e2). Dabei werden die Koeffizienten multipliziert (k1 * k2)
-- und die Exponenten addiert (e1 + e2).
-- Am Ende wird das Ergebnis sauber normalisiert.


> -- ====================================================================
> -- Aufgabe 1f: Polynomdivision mit Rest (/% analog zur Schulmathematik)
> -- ====================================================================
> infix 2 /%
>
> (/%) :: Poly -> Poly -> (Poly, Poly)
> p1 /% p2 = divStep (normalize p1) (normalize p2) []
>   where
>     divStep (P []) _ qAcc = (normalize (P qAcc), P [])
>
>     divStep (P (M kr er : rs)) pDiv@(P (M kd ed : _)) qAcc
>       | er < ed =
>           (normalize (P qAcc), normalize (P (M kr er : rs)))
>
>       | otherwise =
>           let qMonom     = M (kr / kd) (er - ed)
>               subtrahend = mult (P [qMonom]) pDiv
>               nextRest   = add (P (M kr er : rs)) (negat subtrahend)
>           in divStep nextRest pDiv (qMonom : qAcc)

-- Hilfsfunktion für die rekursiven Teilungsschritte.
-- Argumente:
--   Aktueller Rest-Dividend
--   Der Divisor
--   Bisher gesammelte Quotienten-Monome
--
-- Wenn kein Rest mehr vorhanden ist (P []), ist die Division beendet.
--
-- Abbruchbedingung:
-- Ist der höchste Exponent des aktuellen Rests kleiner als der höchste
-- Exponent des Divisors, kann nicht weiter dividiert werden.
-- Der aktuelle Rest wird zum Endrest.
--
-- Rekursionsschritt:
-- 1. Führendes Monom des Rests durch führendes Monom des Divisors teilen.
-- 2. Ergebnis-Monom mit dem gesamten Divisor multiplizieren.
-- 3. Dieses Produkt vom aktuellen Rest abziehen.
-- 4. Mit dem neuen Rest erneut dividieren.
--
-- Dies entspricht exakt der schriftlichen Polynomdivision aus der Mathematik.


> -- ====================================================================
> -- Aufgabe 1g: Erste Ableitung eines Polynoms berechnen
> -- ====================================================================
> derivation :: Poly -> Poly
> derivation (P xs) =
>   normalize (P [ M (k * fromIntegral e) (e - 1) | M k e <- xs, e > 0 ])

-- Erklärung:
-- Wendet die klassische Potenzregel an:
--
--     k*x^e  ->  (k*e)*x^(e-1)
--
-- Der neue Koeffizient wird mit dem Exponenten multipliziert.
-- Der Exponent wird um 1 verringert.
--
-- Die Bedingung "e > 0" filtert konstante Terme heraus,
-- da deren Ableitung immer 0 ist.
--
-- Beispiel:
--     5*x^3 + 2*x
--
-- wird zu:
--     15*x^2 + 2


> -- ====================================================================
> -- Aufgabe 1h: Poly als Instanz der Typklasse Num registrieren
> -- ====================================================================
> instance Num Poly where
>   fromInteger x = P [M (fromInteger x % 1) 0]
>
>   negate p = negat p
>
>   p1 + p2 = add p1 p2
>
>   p1 * p2 = mult p1 p2
>
>   abs (P xs) = error "abs nicht implementiert für Polynome"
>
>   signum _ = error "signum nicht implementiert für Polynome"

-- fromInteger:
-- Wandelt eine normale Zahl in ein konstantes Polynom um.
--
-- Beispiel:
--     5
--
-- wird zu:
--     5*x^0
--
-- negate:
-- Verknüpft das Standard-Minuszeichen mit unserer Funktion negat.
--
-- Beispiel:
--     -(3*x^2)
--
-- wird intern zu:
--     negate (3*x^2)
--
-- und ruft damit:
--     negat (3*x^2)
--
-- auf.
--
-- (+):
-- Verknüpft den Standardoperator + mit unserer add-Funktion.
--
-- (*):
-- Verknüpft den Standardoperator * mit unserer mult-Funktion.
--
-- abs und signum:
-- Für allgemeine Polynome mathematisch nicht sinnvoll definiert.
-- Deshalb werfen sie absichtlich einen Laufzeitfehler.


> example1 = 6 #^ 6 - 2 #^ 5 - 4 #^ 3 + 3 #^ 1 + 3
> example2 = 2 #^ 3 + 2 #^ 1 - 3
>
> res1 = example1 /% example1
> res2 = example1 /% example2

-- Beispielpolynome zum Testen.
--
-- res1:
-- Ein Polynom durch sich selbst geteilt.
-- Ergebnis:
-- Quotient = 1
-- Rest = 0
--
-- res2:
-- Beispiel einer echten Polynomdivision mit Rest.


