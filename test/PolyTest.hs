module Main where 

import Poly
import qualified ParserTest
import qualified LibraryTest
import qualified CacheTest
import qualified GraphTest
import qualified HistoryTest
import qualified ParallelTest

import Test.Tasty
import Test.Tasty.HUnit
import Data.Ratio 


{- 

Hier Testen wir die Funktionen aus Poly.hs mithilfe von Tasty und HUnit.

Mit defaultMain sagen wir, dass wir beim starten des Programms alle Tests ausführen wollen, 
bzw. die Funktion "tests" ausführen wollen.

-}

main :: IO ()
main = defaultMain $
  testGroup "Functional Polynomial Interpreter Tests"
    [ tests
    , ParserTest.tests
    , LibraryTest.tests
    , CacheTest.tests
    , GraphTest.tests
    , HistoryTest.tests
    , ParallelTest.tests
    ]

{-

Um die Tests (PolyTests,ParserTests usw...) auszuführen, muss in der Konsole eingegeben werden:
cabal test --builddir=D:\HochschuleRheinMain\4.Semester\FP\cabal-build-fpi
Der Build-Ordner wird extra angegeben, weil Cabal auf Windows mit dem langen Projektpfad Probleme machen kann.

testGroup ist eine Funktion, die eine Gruppe von Tests zusammenfasst (Liste von Tests).
testCase ist eine Funktion, die einen einzelnen Testfall beschreibt.

vor dem $ steht der Name des Testfalls, nach dem $ steht der eigentliche Test, der ausgeführt wird.
@?= ist ein Operator, der zwei Werte vergleicht und einen Test fehlschlagen lässt, wenn sie nicht gleich sind, also tatsächlichesErgebnis @?= erwartetesErgebnis.


-}

tests :: TestTree
tests =
  testGroup "Poly Tests"

    [

     {- Tests für add: -}
      testCase "addiert zwei Polynome mit unterschiedlichen Exponenten" $
        add (P [M 2 3]) (P [M 4 1]) @?= P [M 2 3, M 4 1]

    , testCase "addiert zwei Polynome mit gleichen Exponenten" $
        add (P [M 2 3]) (P [M 4 3]) @?= P [M 6 3]

    , testCase "addiert zwei Polynome mit gleichen Exponenten, bei der die Summe der Koeffizienten 0 ist" $
        add (P [M 4 3]) (P [M (-4) 3]) @?= P []

    {- Tests für normalize: -}
    , testCase "entfernt Nullmonom" $
        normalize (P [M 0 2]) @?= P []

    , testCase "entfernt Nullmonom aus mehreren Monomen" $
        normalize (P [M 0 2, M 3 1, M 0 5]) @?= P [M 3 1]

    , testCase "fasst Monome mit gleichem Exponenten zusammen" $
        normalize (P [M 2 3, M 4 3]) @?= P [M 6 3]

    , testCase "fasst Monome mit gleichem Exponenten zusammen, bei der die Summe der Koeffizienten 0 ist" $
        normalize (P [M 2 3 , M (-2) 3]) @?= P []

    , testCase "sortiert Monome nach Exponenten absteigend" $
        normalize (P [M 4 1, M 2 3, M 5 2]) @?= P [M 2 3, M 5 2, M 4 1]

    {- Tests für negat: -}
    , testCase "negiert ein Polynom mit einem Monom" $
        negat (P [M 2 3]) @?= P [M (-2) 3]

    , testCase "negiert ein Polynom mit mehreren Monomen" $
        negat (P [M 2 3, M 4 1]) @?= P [M (-2) 3, M (-4) 1]

    , testCase "negiert ein Polynom mit Monomen, welche negative Koeffizienten haben" $
        negat (P [M (-2) 3, M (-4) 1]) @?= P [M 2 3, M 4 1]

    {- Tests für sub: -}
    , testCase "subtrahiert zwei Polynome mit gleichen Exponenten" $
        sub (P [M 5 3]) (P [M 2 3]) @?= P [M 3 3]

    , testCase "subtrahiert zwei Polynome mit unterschiedlichen Exponenten" $
        sub (P [M 5 3]) (P [M 2 1]) @?= P [M 5 3, M (-2) 1]

    , testCase "subtrahiert zwei gleiche Polynome und ergibt Nullpolynom" $
        sub (P [M 5 3, M 2 1]) (P [M 5 3, M 2 1]) @?= P []

    {- Tests für mult: -}
    , testCase "multipliziert zwei Polynome mit jeweils einem Monom" $
        mult (P [M 2 1]) (P [M 3 2]) @?= P [M 6 3]

    , testCase "multipliziert zwei Polynome mit mehreren Monomen" $
        mult (P [M 1 1, M 1 0]) (P [M 1 1, M 2 0]) @?= P [M 1 2, M 3 1, M 2 0]

    , testCase "multipliziert ein Polynom mit dem Nullpolynom" $
        mult (P [M 2 1, M 3 0]) (P []) @?= P []

    {- Tests für derivation: -}
    , testCase "leitet ein Polynom mit einem Monom ab" $
        derivation (P [M 3 2]) @?= P [M 6 1]

    , testCase "leitet ein Polynom mit mehreren Monomen ab" $
        derivation (P [M 4 3, M 2 1, M 5 0]) @?= P [M 12 2, M 2 0]

    , testCase "leitet eine Konstante ab und ergibt Nullpolynom" $
        derivation (P [M 5 0]) @?= P []

    {- Tests für evaluate: -}
    , testCase "wertet ein Polynom an der Stelle x = 2 aus" $
        evaluate (P [M 3 2, M 2 1, M 1 0]) 2 @?= 17

    , testCase "wertet ein konstantes Polynom aus" $
        evaluate (P [M 5 0]) 10 @?= 5

    , testCase "wertet das Nullpolynom aus" $
        evaluate (P []) 10 @?= 0

    {- Tests für division: -}
    , testCase "dividiert x^2 - 1 durch x - 1" $
        (P [M 1 2, M (-1) 0] /% P [M 1 1, M (-1) 0]) @?= (P [M 1 1, M 1 0], P [])

    , testCase "dividiert ein Polynom mit Rest" $
        (P [M 1 2, M 1 0] /% P [M 1 1, M 1 0]) @?= (P [M 1 1, M (-1) 0], P [M 2 0])

    , testCase "dividiert ein Polynom durch sich selbst" $
        (P [M 3 2, M 2 1, M 1 0] /% P [M 3 2, M 2 1, M 1 0]) @?= (P [M 1 0], P [])

     {- Tests für Num-Instanz: -}
    , testCase "fromInteger 0 ergibt Nullpolynom" $
        (0 :: Poly) @?= P []

    , testCase "fromInteger erzeugt konstantes Polynom" $
        (5 :: Poly) @?= P [M 5 0]

    , testCase "Num-Instanz benutzt Addition mit +" $
        ((1 #^ 1) + (1 #^ 1)) @?= P [M 2 1]

    , testCase "Num-Instanz benutzt Subtraktion mit -" $
        ((5 #^ 1) - (2 #^ 1)) @?= P [M 3 1]

    , testCase "Num-Instanz benutzt Multiplikation mit *" $
        ((2 #^ 1) * (3 #^ 2)) @?= P [M 6 3]

    , testCase "Num-Instanz benutzt negate bei unaerem Minus" $
        (-(3 #^ 2)) @?= P [M (-3) 2]

    {- Tests für toLaTeX: -}
    , testCase "gibt ein einfaches Polynom als LaTeX aus" $
        toLaTeX (P [M 3 2, M 2 1, M 1 0]) @?= "3*x^{2}+2*x+1"

    , testCase "gibt ein Polynom mit negativem Koeffizienten als LaTeX aus" $
        toLaTeX (P [M (-3) 2, M 2 1]) @?= "-3*x^{2}+2*x"

    , testCase "gibt ein Polynom mit Bruch als Koeffizienten als LaTeX aus" $
        toLaTeX (P [M (3 % 5) 1]) @?= "\\frac{3}{5}*x"

    ]
