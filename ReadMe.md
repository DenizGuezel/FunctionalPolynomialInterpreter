# Functional Polynomial Interpreter

Das Projekt ist ein kleiner Polynomrechner in Haskell mit einer einfachen GUI.

Man kann Polynome eingeben, in einer Liste speichern und dann verschiedene Operationen darauf anwenden. Die Ergebnisse können danach unterschiedlich angezeigt werden, z.B. als normales Ergebnis, als LaTeX-Ausgabe, als Baum, als Analyse oder als Graph.

## Ziel

Das Ziel war, ein Projekt zu bauen, in dem mehrere Themen aus der Vorlesung praktisch benutzt werden. Der mathematische Teil wird dabei selbst umgesetzt und nicht durch eine fertige Mathematikbibliothek ersetzt.

Ein Polynom wie:
3x² + 2x + 1

wird im Programm z.B. so eingegeben:
3 2; 2 1; 1 0

Intern wird daraus dann ein eigener Haskell-Datentyp:
P [M (3 % 1) 2, M (2 % 1) 1, M (1 % 1) 0]

## Funktionen

Aktuell kann das Programm:

- Polynome hinzufügen und in einer Liste speichern
- Polynome normalisieren
- Polynome negieren
- Polynome addieren, subtrahieren, multiplizieren und dividieren
- Polynome ableiten
- Polynome an einem x-Wert auswerten
- Ergebnisse als LaTeX anzeigen
- einen Ausdrucksbaum anzeigen
- eine Baum-Analyse anzeigen
- Traversierungsschritte anzeigen
- Details zu einem Ergebnis anzeigen
- einen einfachen Graphen mit Wertetabelle anzeigen
- eine Historie der Berechnungen anzeigen
- Ergebnisse intern im Cache wiederverwenden
- zufällige Polynome erzeugen
- mehrere Polynome parallel auswerten
- feste Beispielpolynome laden

## Eingabe

Ein Monom besteht immer aus Koeffizient und Exponent:

Koeffizient Exponent

Mehrere Monome werden mit einem Semikolon getrennt.

Beispiele:

3 2; 2 1; 1 0     entspricht  3x² + 2x + 1
1 1; -4 0         entspricht  x - 4
5 3               entspricht  5x³
2 0               entspricht  2

Negative Koeffizienten sind erlaubt. Negative Exponenten werden nicht akzeptiert, da in diesem Projekt nur Polynome behandelt werden.

## Aufbau

Die wichtigsten Dateien liegen im Ordner src.

Poly.hs enthält die Grundlogik für Polynome und Monome. Dort sind auch die wichtigsten Rechenoperationen umgesetzt.

ParserSimple.hs wandelt die Texteingabe in Polynome um.

Format.hs kümmert sich um mathematische Darstellung, LaTeX und Baumformatierung.

Display.hs baut daraus die Texte, die später in der GUI angezeigt werden.

Tree.hs, Analysis.hs und Animation.hs gehören zur Baumdarstellung und zu den Traversierungsschritten.

Graph.hs erzeugt Punkte für den Graphen und die Wertetabelle. Dort wird auch Lazy Evaluation sichtbar benutzt.

History.hs speichert vergangene Berechnungen.

Library.hs speichert die Polynome, die der Benutzer in der Polynomliste hat.

Cache.hs speichert bereits berechnete Ergebnisse intern.

Parallel.hs enthält die normale und die parallele Auswertung mehrerer Polynome.

Random.hs erzeugt zufällige Polynome.

Template.hs und Examples.hs enthalten die Template-Haskell-Beispiele.

GUI.hs verbindet die Funktionen mit der Threepenny-GUI.

Eine ausführlichere Beschreibung der Module kommt später noch in den Ordner docs.

## Vorlesungsthemen

Im Projekt werden mehrere funktionale Konzepte benutzt.

Algebraische Datentypen werden z.B. für Monom, Poly, ExprTree, History und Operation verwendet.

Pattern Matching kommt an vielen Stellen vor, z.B. beim Zerlegen von Polynomen, Monomen, Bäumen und Parser-Ergebnissen.

Rekursion wird unter anderem bei Listenfunktionen, der History und der Baumdarstellung benutzt.

Funktionen wie map, filter, foldr und take werden für die Verarbeitung von Listen eingesetzt.

Mit der Typklasse Pretty werden verschiedene Werte einheitlich lesbar dargestellt. Außerdem besitzt Poly eine Num-Instanz, damit man Polynome auch mit +, - und * verwenden kann.

Für Fehlerfälle werden Either und Maybe benutzt, z.B. beim Parser oder beim Suchen in der Polynomliste.

Die GUI arbeitet mit IO und IORef, da dort Benutzereingaben und Zustände verwaltet werden müssen.

Lazy Evaluation wird bei der Graphfunktion benutzt. Dort wird eine unendliche Liste von x-Werten erzeugt, aus der später nur ein kleiner sichtbarer Bereich genommen wird.

Parallelisierung wird bei der Auswertung mehrerer Polynome verwendet.

Template Haskell wird für feste Beispielpolynome genutzt, die beim Kompilieren erzeugt werden.

## Starten

Im Projektordner kann die Anwendung so gestartet werden:

cabal run FunctionalPolynomialInterpreter

Falls es unter Windows Probleme mit langen Pfaden gibt, kann ein eigener Build-Ordner verwendet werden:

cabal run FunctionalPolynomialInterpreter --builddir=D:\HochschuleRheinMain\4.Semester\FP\cabal-build-fpi

Nach dem Start öffnet sich die GUI im Browser.

## Tests

Die Tests werden mit Cabal ausgeführt:

cabal test

Oder mit dem extra Build-Ordner:

cabal test --builddir=D:\HochschuleRheinMain\4.Semester\FP\cabal-build-fpi

Aktuell laufen 84 Tests erfolgreich.

Getestet werden unter anderem:

- Polynomoperationen
- Parser
- Polynomliste
- Cache
- Graph und Wertetabelle
- History
- Parallelisierung

## Beispielablauf

1. Ein Polynom eingeben, z.B.:

   3 2; 2 1; 1 0

2. Auf Polynom hinzufügen klicken.
3. Das Polynom in der Liste auswählen.
4. Eine Operation ausführen, z.B. Ableiten oder Auswerten.
5. Danach eine Darstellung auswählen, z.B. Baum, LaTeX, Graph oder Details.

## Fehlerfälle

Einige typische Fehler werden abgefangen, z.B.:

- leere Eingabe
- falsches Eingabeformat
- ungültige Zahlen
- negative Exponenten
- zu wenige oder zu viele ausgewählte Polynome
- Division durch das Nullpolynom
- fehlender x-Wert bei der Auswertung
- keine vorhandene Berechnung für eine Darstellungsfunktion

## Tests im Projekt

Die Tests liegen im Ordner test.

PolyTest.hs ist der Einstiegspunkt für die Tests.

Weitere Testmodule sind:

- ParserTest.hs
- LibraryTest.hs
- CacheTest.hs
- GraphTest.hs
- HistoryTest.hs
- ParallelTest.hs

## Lizenz

MIT License
