module Main where 

import Poly 
import Test.Tasty
import Test.Tasty.HUnit

{- 

Hier Testen wir die Funktionen aus Poly.hs mithilfe von HUnit 

Mit defaultMain sagen wir, dass wir beim starten des Programms alle Tests ausführen wollen, 
bzw. die Funktion "tests" ausführen wollen.

-}

main :: IO ()
main = defaultMain tests

{-

Um die Tests auszuführen, muss in der Konsole eingegeben werden:
cabal test --builddir=D:\HochschuleRheinMain\4.Semester\FP\cabal-build-fpi
Weil die Tests in einem anderen Build-Ordner liegen, als das normale Projekt.

testGroup ist eine Funktion, die eine Gruppe von Tests zusammenfasst (Liste von Tests).
testCase ist eine Funktion, die einen einzelnen Testfall beschreibt.

vor dem $ steht der Name des Testfalls, nach dem $ steht der eigentliche Test, der ausgeführt wird.
@?= ist ein Operator, der zwei Werte vergleicht und einen Test fehlschlagen lässt, wenn sie nicht gleich sind, also tatsächlichesErgebnis @?= erwartetesErgebnis.


-}

tests :: TestTree
tests =
  testGroup "Poly Tests"

    [ 

     {- Tests für Add: -}
      testCase "addiert zwei Polynome mit unterschiedlichen Exponenten" $
        add (P [M 2 3]) (P [M 4 1]) @?= P [M 2 3, M 4 1]

    , testCase "addiert zwei Polynome mit gleichen Exponenten" $
        add (P [M 2 3]) (P [M 4 3]) @?= P [M 6 3]

    , testCase "addiert zwei Polynome mit gleichen Exponenten, bei der die Summe der Koeffizienten 0 ist" $
        add (P [M 4 3]) (P [M (-4) 3]) @?= P []
    
    {- Tests für Normalize: -}
    , testCase " entfernt Nullmonom" $
        normalize (P [M 0 2]) @?= P []

    , testCase "entfernt Nullmonom aus mehreren Monomen" $
        normalize (P [M 0 2, M 3 1, M 0 5]) @?= P [M 3 1]

    , testCase "fasst Monome mit gleichem Exponenten zusammen" $
        normalize (P [M 2 3, M 4 3]) @?= P [M 6 3]
    
    , testCase "fasst Monome mit gleichem Exponenten zusammen, bei der die Summe der Koeffizienten 0 ist" $
        normalize (P [M 2 3 , M (-2) 3]) @?= P []

    ]

