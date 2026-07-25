module Parallel where

import Poly
import Control.Parallel.Strategies (parMap, rdeepseq)

{- Dieses Modul dient dazu, mehrere Polynome gleichzeitig zu verarbeiten. -}

{-

Dieser Typ steht für eine Polynomliste mit Namen.

Ein Eintrag besteht aus dem Namen des Polynoms und dem Polynom selbst.
Also zum Beispiel: ("p1", P [M 3 2, M 2 1])

Dadurch kann die parallele Auswertung später in der GUI wieder verständlich angezeigt werden.

-}

type NamedPolys = [(String, Poly)]

{-

Dieser Typ steht für die Ergebnisse einer Auswertung.

Ein Eintrag besteht aus dem Namen des Polynoms und dem berechneten Wert.
Also zum Beispiel: ("p1", 17)

-}

type NamedResults = [(String, Rational)]

{- 

Diese Funktion dient dazu, mehrere Polynome zu evaluieren. 
Sie nimmt einen Wert x und eine Liste von Polynomen und gibt eine Liste von Rationalen zurück, 
die die Ergebnisse der Evaluierung jedes Polynoms an der Stelle x enthalten.

-}

evaluateMany :: Rational -> [Poly] -> [Rational]
evaluateMany x polys = map (\poly -> evaluate poly x) polys

{-

Diese Funktion wertet mehrere benannte Polynome nacheinander aus.

Sie ist die sequentielle Referenzversion zur parallelen Auswertung.
Das bedeutet: Die Polynome werden der Reihe nach mit map verarbeitet.

Diese Funktion ist wichtig, weil wir damit zeigen können, dass die parallele Version fachlich dasselbe Ergebnis liefert.

-}

evaluateNamedMany :: Rational -> NamedPolys -> NamedResults
evaluateNamedMany x namedPolys = map (\(name, poly) -> (name, evaluate poly x)) namedPolys

{- 

Diese Funktion tut im Wesentlichen dasselbe wie evaluateMany, aber sie nutzt die Parallelität, 
um die Evaluierung der Polynome zu beschleunigen.

parMap verteilt die unabhängigen Auswertungen auf mehrere parallele Berechnungen.
rdeepseq sorgt dafür, dass die Ergebnisse vollständig ausgewertet werden.

Das passt hier gut, weil die Auswertung von p1, p2, p3 usw. nicht voneinander abhängt.

-}

evaluateManyParallel :: Rational -> [Poly] -> [Rational]
evaluateManyParallel x polys = parMap rdeepseq (\poly -> evaluate poly x) polys


{- 

Diese Funktion ist eine Erweiterung von evaluateManyParallel.

Sie wertet mehrere Polynome parallel aus und behält dabei die Namen der Polynome.
Dadurch kann die GUI danach z.B. "p1: 12" und "p2: 20" anzeigen.

-}

evaluateNamedManyParallel :: Rational -> NamedPolys -> NamedResults
evaluateNamedManyParallel x namedPolys = parMap rdeepseq (\(name, poly) -> (name, evaluate poly x)) namedPolys

{-

Diese Funktion führt die sequentielle und die parallele Auswertung aus.

Sie gibt drei Dinge zurück:

1. Die sequentiellen Ergebnisse.
2. Die parallelen Ergebnisse.
3. Einen Wahrheitswert, ob beide Ergebnislisten gleich sind.

Dadurch kann in der GUI gezeigt werden, dass die parallele Version korrekt ist.
Das ist für die Präsentation stärker, weil wir nicht nur parallel rechnen, sondern die Korrektheit gegen die einfache map-Version prüfen.

-}

compareSequentialAndParallel :: Rational -> NamedPolys -> (NamedResults, NamedResults, Bool)
compareSequentialAndParallel x namedPolys =
   let sequentialResults = evaluateNamedMany x namedPolys
       parallelResults = evaluateNamedManyParallel x namedPolys
   in (sequentialResults, parallelResults, sequentialResults == parallelResults)