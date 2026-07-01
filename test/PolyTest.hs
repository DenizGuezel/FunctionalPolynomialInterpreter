module Main where 

import Poly 
import Test.Tasty
import Test.Tasty.HUnit

{- 

Hier Testen wir die Funktionen aus Poly.hs mithilfe von HUnit 

Mit defaultMain sagen wir, dass wir beim starten des Programms alle Tests ausführen wollen, 
bzw. die Funktion "tests" ausführen wollen:l.

-}

main :: IO ()
main = defaultMain tests

{-

testGroup ist eine Funktion, die eine Gruppe von Tests zusammenfasst (Liste von Tests).
testCase ist eine Funktion, die einen einzelnen Testfall beschreibt.

vor dem $ steht der Name des Testfalls, nach dem $ steht der eigentliche Test, der ausgeführt wird.
@?= ist ein Operator, der zwei Werte vergleicht und einen Test fehlschlagen lässt, wenn sie nicht gleich sind, also tatsächlichesErgebnis @?= erwartetesErgebnis.


-}

tests :: TestTree
tests =
  testGroup "Poly Tests"
    [ testCase "normalize entfernt Nullmonom" $
        normalize (P [M 0 2]) @?= P []
    ]

