module ParserTest where

import ParserSimple
import Poly

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus ParserSimple.hs.

Der Parser ist wichtig für die GUI, weil die Eingabe des Benutzers zuerst von String
in unsere internen Datentypen Monom und Poly umgewandelt wird.

Dabei testen wir gültige Eingaben und bewusst falsche Eingaben.
So stellen wir sicher, dass der Parser nicht abstürzt, sondern saubere Fehlermeldungen zurückgibt.

-}

tests :: TestTree
tests =
  testGroup "Parser Tests"

    [

    {- Tests für gültige Monom- und Polynom-Eingaben: -}

      testCase "parst ein einzelnes Monom" $
        parseMonomSimple "3 2" @?= Right (M 3 2)

    , testCase "parst ein Polynom mit mehreren Monomen" $
        parsePolySimple "3 2;5 1;7 0" @?= Right (P [M 3 2, M 5 1, M 7 0])

    , testCase "parst ein Polynom und normalisiert gleiche Exponenten" $
        parsePolySimple "2 1;3 1" @?= Right (P [M 5 1])

    , testCase "parst ein Polynom und entfernt Nullmonome durch normalize" $
        parsePolySimple "0 5;3 2" @?= Right (P [M 3 2])

    , testCase "parst negative Koeffizienten" $
        parsePolySimple "-3 2;5 1" @?= Right (P [M (-3) 2, M 5 1])

    {- Tests für ungültige Eingaben: -}

    , testCase "gibt Fehler bei leerem Monom" $
        parseMonomSimple "" @?=
          Left "Fehler: Zerlegte Liste ist leer, es wurden keine Zahlen gefunden."

    , testCase "gibt Fehler bei ungueltigem Koeffizienten" $
        parseMonomSimple "abc 2" @?=
          Left "Fehler: Koeffizient 'abc' ist keine gültige Zahl."

    , testCase "gibt Fehler bei ungueltigem Exponenten" $
        parseMonomSimple "3 abc" @?=
          Left "Fehler: Exponent 'abc' ist keine gültige ganze Zahl."

    , testCase "gibt Fehler bei negativem Exponenten" $
        parseMonomSimple "3 -1" @?=
          Left "Fehler: Exponent '-1' darf nicht negativ sein."

    , testCase "gibt Fehler bei zu vielen Eingabeteilen" $
        parseMonomSimple "3 2 1" @?=
          Left "Fehler: Ein Monom muss genau aus Koeffizient und Exponent bestehen. Gefunden wurde: [\"3\",\"2\",\"1\"]"

    ]
