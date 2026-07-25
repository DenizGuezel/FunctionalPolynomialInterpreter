module ParallelTest where

import Parallel
import Poly

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus Parallel.hs.

Die parallele Auswertung soll fachlich dasselbe Ergebnis liefern wie die normale sequentielle Auswertung.
Der Unterschied liegt nur darin, dass die unabhängigen Polynom-Auswertungen parallel berechnet werden können.

Wir testen deshalb besonders, dass sequentielle und parallele Ergebnisse gleich sind.

-}

tests :: TestTree
tests =
  testGroup "Parallel Tests"

    [
      testCase "wertet mehrere Polynome sequentiell aus" $
        evaluateMany 2 [P [M 3 2], P [M 1 1, M 1 0]] @?= [12, 3]

    , testCase "wertet mehrere Polynome parallel aus" $
        evaluateManyParallel 2 [P [M 3 2], P [M 1 1, M 1 0]] @?= [12, 3]

    , testCase "wertet benannte Polynome sequentiell aus" $
        let namedPolys = [("p1", P [M 3 2]), ("p2", P [M 1 1, M 1 0])]
        in evaluateNamedMany 2 namedPolys @?= [("p1", 12), ("p2", 3)]

    , testCase "wertet benannte Polynome parallel aus" $
        let namedPolys = [("p1", P [M 3 2]), ("p2", P [M 1 1, M 1 0])]
        in evaluateNamedManyParallel 2 namedPolys @?= [("p1", 12), ("p2", 3)]

    , testCase "vergleicht sequentielle und parallele Auswertung erfolgreich" $
        let namedPolys = [("p1", P [M 3 2]), ("p2", P [M 1 1, M 1 0])]
        in compareSequentialAndParallel 2 namedPolys @?=
             ([("p1", 12), ("p2", 3)], [("p1", 12), ("p2", 3)], True)

    ]
