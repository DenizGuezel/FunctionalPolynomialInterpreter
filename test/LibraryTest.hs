module LibraryTest where

import Library
import Poly

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus Library.hs.

Die Library ist wichtig für die GUI, weil dort die Polynome gespeichert werden,
die der Benutzer später auswählen und wiederverwenden kann.

Wir testen deshalb vor allem Speichern, Suchen, Löschen, Auswählen und die automatische Namensvergabe.

-}

tests :: TestTree
tests =
  testGroup "Library Tests"

    [

    {- Tests für Speichern und Suchen: -}

      testCase "speichert ein Polynom in der Library" $
        savePoly "p1" (P [M 3 2]) [] @?= [("p1", P [M 3 2])]

    , testCase "überschreibt ein vorhandenes Polynom mit gleichem Namen" $
        savePoly "p1" (P [M 5 1]) [("p1", P [M 3 2])] @?= [("p1", P [M 5 1])]

    , testCase "findet ein gespeichertes Polynom" $
        lookupPoly "p2" [("p1", P [M 1 1]), ("p2", P [M 2 2])] @?= Just (P [M 2 2])

    , testCase "gibt Nothing zurück, wenn ein Polynom nicht existiert" $
        lookupPoly "p3" [("p1", P [M 1 1])] @?= Nothing

    {- Tests für Löschen und Auflisten: -}

    , testCase "löscht ein Polynom aus der Library" $
        deletePoly "p1" [("p1", P [M 1 1]), ("p2", P [M 2 2])] @?= [("p2", P [M 2 2])]

    , testCase "lässt die Library unverändert, wenn der Name nicht existiert" $
        deletePoly "p3" [("p1", P [M 1 1])] @?= [("p1", P [M 1 1])]

    , testCase "listet alle Polynomnamen auf" $
        listPolys [("p1", P [M 1 1]), ("p2", P [M 2 2])] @?= ["p1", "p2"]

    {- Tests für automatische Namen und Auswahl: -}

    , testCase "erzeugt den nächsten freien p-Namen" $
        nextPolyName [("p1", P [M 1 1]), ("p3", P [M 3 3])] @?= "p2"

    , testCase "erzeugt den nächsten freien r-Namen" $
        nextRandomName [("r1", P [M 1 1]), ("r2", P [M 2 2])] @?= "r3"

    , testCase "wählt genau die ausgewählten Polynome aus" $
        selectedPolys [("p1", P [M 1 1]), ("p2", P [M 2 2]), ("p3", P [M 3 3])] ["p1", "p3"] @?=
          [("p1", P [M 1 1]), ("p3", P [M 3 3])]

    , testCase "behaelt die Reihenfolge der Auswahl bei" $
        selectedPolys [("p1", P [M 1 1]), ("b1", P [M 2 2])] ["b1", "p1"] @?=
          [("b1", P [M 2 2]), ("p1", P [M 1 1])]

    ]
