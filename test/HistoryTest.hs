module HistoryTest where

import History
import Poly
import Format (pretty)

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus History.hs.

Die History speichert vergangene Berechnungen.
Dadurch kann die GUI anzeigen, welche Ergebnisse vorher berechnet wurden.

Wir testen deshalb Hinzufügen, Umwandeln in Listen, letzten Eintrag, Undo, Leeren und Pretty-Ausgabe.

-}

tests :: TestTree
tests =
  testGroup "History Tests"

    [

      testCase "fügt einen Eintrag in eine leere Historie ein" $
        addHistory "a" Empty @?= Entry "a" Empty

    , testCase "wandelt eine Historie in eine Liste um" $
        historyToList (Entry "b" (Entry "a" Empty)) @?= ["b", "a"]

    , testCase "gibt Nothing bei leerer Historie zurück" $
        latestHistory (Empty :: History String) @?= Nothing

    , testCase "gibt den zuletzt hinzugefügten Eintrag zurück" $
        latestHistory (Entry "b" (Entry "a" Empty)) @?= Just "b"

    , testCase "entfernt den zuletzt hinzugefügten Eintrag" $
        undoHistory (Entry "b" (Entry "a" Empty)) @?= Entry "a" Empty

    , testCase "leert die gesamte Historie" $
        clearHistory (Entry "b" (Entry "a" Empty)) @?= (Empty :: History String)

    , testCase "stellt ein Polynom-Ergebnis lesbar dar" $
        pretty (HistoryPoly "p1" (P [M 3 2, M 2 1])) @?= "p1: 3x² + 2x"

    , testCase "stellt ein Wert-Ergebnis lesbar dar" $
        pretty (HistoryValue "p1(2)" 10) @?= "p1(2): 10"

    , testCase "stellt ein Divisionsergebnis lesbar dar" $
        pretty (HistoryDiv "p1 / p2" (P [M 1 1]) (P [M 2 0])) @?=
          "p1 / p2: Quotient = x, Rest = 2"

    ]
