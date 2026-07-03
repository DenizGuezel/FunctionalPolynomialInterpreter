module ParserSimple where 

{- 

Ziel des ParserSimple ist es aus einem String einen Polynom-Ausdruck zu parsen, 
das ist genau andersrum als die toLatex Funktion, die aus einem Polynom-Ausdruck einen String erzeugt.

-}

import Poly
import Data.Ratio
import Text.Read(readMaybe)
import qualified Control.Applicative as Fälle


{- 

Das ist unsere Hauptfunktion, welche einen String, der ein Polynom beschreibt, in ein Polynom (Poly) parsen soll.
Die Funktion bekommt z.B als Eingabe: "3 2;5 1;7 0" und gibt als Ausgabe Either String Poly zurück, das entweder ein Fehler-String oder ein Polynom (Poly) ist.

Either ist ein Datentyp, der entweder einen Wert vom Typ Left a (für uns hier Fehlermeldung) oder Right b (hier ein Polynom) enthält und somit eine sichere Art ist, 
entweder einen Fehler oder einen gültigen Wert zurückzugeben.

-}

parsePolySimple :: String -> Either String Poly
parsePolySimple s = parseMonomList (splitBySemicolon s)

{- 

Diese Funktion soll eine Liste von Strings, die Monome beschreiben, in ein Polynom parsen.
Sie bekommt als Eingabe z.B ["3 2", "5 1", "7 0"] und gibt als Ausgabe Either String Poly zurück, das entweder ein Fehler-String oder ein Polynom (Poly) ist.

Falls eine leere Liste übergeben wird, bekommen wir ein Ergebnis vom Typ Either mit einem Polynom, das keine Monome enthält (P []).

Falls eine nicht Leere Liste übergeben wird, führen wir die Funktion parseMonomSimple auf das erste Element der Liste aus und prüfen, ob das Ergebnis ein Fehler ist (Left) oder ein gültiges Monom (Right).
Falls bei der Ausführung von parseMonomSimple ein Fehler auftritt, geben wir diesen Fehler zurück (Left err).
Wenn kein Fehler auftritt, bekommen wir ein gültiges Monom (Right monom) und führen die Funktion parseMonomList rekursiv auf den Rest der Liste aus.

Falls bei der Ausführung auf der Resliste ein Fehler auftritt, geben wir diesen Fehler zurück (Left err).
Wenn kein Fehler auftritt (die Restliste erfolgreich geparst wurde), bekommen wir ein gültiges Polynom (Right (P monoms)) und fügen das Monom, das wir vorher geparst haben, zu der Liste der Monome hinzu und geben das Ergebnis als Polynom zurück (Right (normalize (P (monom:monoms)))).

case führt eine Funktion aus, hier z.B parseMonomList x unf prüft mit of, welches Ergebnismuster in den unteren Zeilen zutrifft.

-}

parseMonomList :: [String] -> Either String Poly 
parseMonomList [] = Right (P [])
parseMonomList (x:xs) = case parseMonomSimple x of 
    Left err -> Left err 
    Right monom -> case parseMonomList xs of 
        Left err -> Left err
        Right (P monoms) -> Right (normalize (P (monom:monoms)))

{- 

Diese Funktion soll einen String, der ein einzelnes Monom beschreibt, in ein Monom parsen.
Sie bekommt als Eingabe z.B "3 2" und gibt als Ausgabe Either String Monom zurück, das entweder ein Fehler-String oder ein Monom ist.

Wir führen die Funktion words auf den Eingabe-String aus, um ihn in eine Liste von Strings zu zerlegen und prüfen welche der unteren Fälle zutreffen.
words input zerlegt den übergebenen Parameter-String (z.B "3 2") in eine Liste von Strings (z.B ["3", "2"]).

Wenn die zerlegte Liste leer ist, geben wir einen Fehler zurück.

Wenn die zerlegte Liste genau zwei Elemente (den Koeffizienten und den Exponenten) enthält, führen wir die Funktion parseNumbers auf diese beiden Elemente aus, 
um sie in ein Monom zu parsen und prüfen erneut, welche der restlichen zwei Fälle dann zutreffen. 
Wenn bei dem Parsen der Zahlen ein Fehler auftritt, geben wir diesen Fehler zurück (Left err).
Wenn kein Fehler auftritt, bekommen wir ein gültiges Monom (Right monom) und geben es als Ergebnis zurück.

Wenn keiner der Fälle zutrifft (also die zerlegte Liste mehr als zwei Elemente enthält oder nur eine Zahl), geben wir einen Fehler zurück, der besagt, 
dass genau zwei Zahlen erwartet wurden, aber mehr gefunden wurden.

-}

parseMonomSimple :: String -> Either String Monom
parseMonomSimple input =  case words input of 
    [] -> Left "Fehler: Zerlegte Liste ist leer, es wurden keine Zahlen gefunden."
    [coeff, exp] -> case (parseNumbers coeff exp) of
        Left err -> Left err
        Right monom -> Right monom
    other -> Left ("Fehler: Erwartet wurden genau zwei Zahlen, aber es wurden " ++ show (length other) ++ " gefunden: " ++ unwords other)    

{- 

Diese Funktion soll zwei Strings, die den Koeffizienten und den Exponenten eines Monoms beschreiben, in ein Monom parsen.
Sie bekommt als Eingabe z.B "3" und "2" und gibt als Ausgabe Either String Monom zurück, das entweder ein Fehler-String oder ein Monom ist.

Es wird versucht, den Koeffizienten-String und den Exponenten-String in die entsprechenden Typen (Rational und Int) zu parsen.
Der Einleseverusch kann durch den Datentyp Maybe entweder erfolgreich sein (Just c, Just e) oder fehlschlagen (Nothing).

Dieser case wird ausgeführt und es wird nach den 3 unteren Mustern geprüft, welche zutreffen.

Fall 1: Wenn beide Einleseversuche erfolgreich waren (Just c, Just e), wird ein Monom mit dem Koeffizienten c und dem Exponenten e erstellt und als Right-Wert zurückgegeben.
Fall 2: Wenn der Einleseversuch für den Koeffizienten fehlschlägt (Nothing), für den Exponenten aber erfolgreich ist, wird eine Fehlermeldung zurückgegeben, die besagt, dass der Koeffizient keine gültige Zahl ist.
Fall 3: Wenn der Einleseversuch für den Exponenten fehlschlägt (Nothing), für den Koeffizienten aber erfolgreich ist, wird eine Fehlermeldung zurückgegeben, die besagt, dass der Exponent keine gültige ganze Zahl ist.

-}

parseNumbers :: String -> String -> Either String Monom
parseNumbers coeff exp = case (readMaybe coeff :: Maybe Rational, readMaybe exp :: Maybe Int) of
    (Just c, Just e) -> Right (M c e)
    (Nothing, _) -> Left ("Fehler: Koeffizient '" ++ coeff ++ "' ist keine gültige Zahl.")
    (_, Nothing) -> Left ("Fehler: Exponent '" ++ exp ++ "' ist keine gültige ganze Zahl.") 


{- 

Diese Funktion soll einen String, der mehrere Monome beschreibt, in eine Liste von Strings zerlegen, die jeweils ein Monom beschreiben.
Sie bekommt als Eingabe z.B "3 2;5 1;7 0" und gibt als Ausgabe eine Liste von Strings zurück, die jeweils ein Monom beschreiben, z.B ["3 2", "5 1", "7 0"].

Wenn der Eingabe-String (es ist ein [char], deswegen matchen wir mit []) leer iszt, geben wir eine leere Liste zurück.

Wenn ein gültiger String übergeben wird, gehen wir wie folgt weiter:

Es wird zerlegt, indem der String an jedem Semikolon (;) aufgeteilt wird. 
before ist der Teil des Strings vor dem Semikolon und after ist der Teil des Strings nach dem Semikolon.

z.B haben wir den String "3 2;5 1;7 0", dann ist before = "3 2" und after = ";5 1;7 0".

Wir gucken uns jetzt den after-Teil an und prüfen diesen auf die zwei unteren Fälle.
Fall 1: Wenn after leer ist, bedeutet das, dass es gar kein Semikolon im String gab, also geben wir eine Liste mit nur dem before-Teil zurück.
Fall 2: Wenn after nicht leer ist, bedeutet das, dass es mindestens ein Semikolon im String gab, also geben wir eine Liste zurück, die den before-Teil 
und die rekursive Ausführung der Funktion auf den Rest des after-Teils enthält.

-}

splitBySemicolon :: String -> [String]
splitBySemicolon [] = [] 
splitBySemicolon s = let (before, after) = break (== ';') s
                     in case after of
                          [] -> [before]
                          (_:rest) -> before : splitBySemicolon rest

