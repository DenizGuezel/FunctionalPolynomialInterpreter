module ParserSimple where 

{- 

Ziel des ParserSimple ist es aus einem String einen Polynom-Ausdruck zu parsen, 
das ist genau andersrum als die toLatex Funktion, die aus einem Polynom-Ausdruck einen String erzeugt.

-}

import Poly
import Data.Ratio
import Text.Read(readMaybe)


{- 

Das ist unsere Hauptfunktion, welche einen String, der ein Polynom beschreibt, in ein Polynom (Poly) parsen soll.
Die Funktion bekommt z.B als Eingabe: "3 2;5 1;7 0" und gibt als Ausgabe Either String Poly zurück, das entweder ein Fehler-String oder ein Polynom (Poly) ist.

Either ist ein Datentyp, der entweder einen Wert vom Typ Left a (für uns hier Fehlermeldung) oder Right b (hier ein Polynom) enthält und somit eine sichere Art ist, 
entweder einen Fehler oder einen gültigen Wert zurückzugeben.

-}

parsePolySimple :: String -> Either String Poly
parsePolySimple s = 0

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


