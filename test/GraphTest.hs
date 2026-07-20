module GraphTest where

import Graph
import Poly

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus Graph.hs.

Der Graph nutzt Lazy Evaluation, weil unendliche x-Wert-Listen erzeugt werden,
von denen später nur ein sichtbarer Ausschnitt ausgewertet wird.

Wir testen deshalb sowohl die berechneten Punkte als auch die formatierte Wertetabelle.

-}

tests :: TestTree
tests =
  testGroup "Graph Tests"

    [

      testCase "berechnet Punkte für eine gegebene x-Liste" $
        samplePoints (P [M 1 2]) [-2, -1, 0, 1, 2] @?=
          [(-2, 4), (-1, 1), (0, 0), (1, 1), (2, 4)]

    , testCase "erzeugt einen unendlichen x-Wert-Strom lazy mit iterate" $
        take 5 (xValueStream (-2) 1) @?= [-2, -1, 0, 1, 2]

    , testCase "nimmt nur die sichtbaren Punkte aus der unendlichen Punktliste" $
        visiblePoints 3 (P [M 1 1]) @?= [(-10, -10), (-9, -9), (-8, -8)]

    , testCase "nimmt Punkte mit frei gewähltem Startwert und Schrittweite" $
        visiblePointsFrom 4 0 2 (P [M 1 1]) @?= [(0, 0), (2, 2), (4, 4), (6, 6)]

    , testCase "erzeugt Standardpunkte von -10 bis 10" $
        (head (defaultPoints (P [M 1 1])), last (defaultPoints (P [M 1 1]))) @?=
          ((-10, -10), (10, 10))

    , testCase "formatiert einen Punkt lesbar" $
        formatPoint (2 :: Rational, 4 :: Rational) @?= "2 | 4"

    , testCase "füllt kurze Strings links mit Leerzeichen auf" $
        padLeft 4 "7" @?= "   7"

    , testCase "formatiert eine Tabelle mit ausgerichteten Spalten" $
        formatTable [(-1 :: Rational, -3 :: Rational), (0, 2), (1, 10)] @?=
          " x |  y\n-------\n-1 | -3\n 0 |  2\n 1 | 10\n"

    , testCase "erzeugt SVG-Code für einen Graphen" $
        take 4 (graphSvg (P [M 1 1])) @?= "<svg"

    ]
