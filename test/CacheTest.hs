module CacheTest where

import Cache
import Poly

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus Cache.hs.

Der Cache speichert bereits berechnete Operationen intern.
Dadurch kann die GUI ein Ergebnis wiederverwenden, wenn dieselbe Operation erneut ausgeführt wird.

Wir testen deshalb Einfügen, Suchen, Ersetzen und Entfernen von Cache-Einträgen.

-}

tests :: TestTree
tests =
  testGroup "Cache Tests"

    [
      testCase "findet nichts in einem leeren Cache" $
        lookupCache (Normalize (P [M 1 1])) ([] :: Cache CachedResult) @?= Nothing

    , testCase "fügt ein Polynomergebnis in den Cache ein und findet es wieder" $
        let operation = Normalize (P [M 1 1])
            result = CachedPoly (P [M 1 1])
            cache = insertCache operation result []
        in lookupCache operation cache @?= Just result

    , testCase "unterscheidet verschiedene Operationen" $
        let poly = P [M 2 2]
            cache = insertCache (Normalize poly) (CachedPoly poly) []
        in lookupCache (Negate poly) cache @?= Nothing

    , testCase "ersetzt einen vorhandenen Cache-Eintrag mit gleicher Operation" $
        let operation = Normalize (P [M 1 1])
            oldResult = CachedPoly (P [M 1 1])
            newResult = CachedPoly (P [M 2 2])
            cache = insertCache operation newResult (insertCache operation oldResult [])
        in lookupCache operation cache @?= Just newResult

    , testCase "entfernt einen Cache-Eintrag" $
        let operation = Evaluate (P [M 3 2]) 2
            cache = insertCache operation (CachedValue 12) []
        in lookupCache operation (removeCache operation cache) @?= Nothing

    , testCase "speichert Divisionsergebnisse mit Quotient und Rest" $
        let operation = Div (P [M 1 2, M (-1) 0]) (P [M 1 1, M (-1) 0])
            quotient = P [M 1 1, M 1 0]
            rest = P []
            cache = insertCache operation (CachedDiv quotient rest) []
        in lookupCache operation cache @?= Just (CachedDiv quotient rest)

    ]
