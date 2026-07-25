module TreeTest where

import Tree
import Poly
import Format (pretty)

import Test.Tasty
import Test.Tasty.HUnit

{-

Hier testen wir die Funktionen aus Tree.hs.

Tree.hs ist für die Ausdrucksbäume zuständig.
Diese Bäume werden später in der GUI angezeigt und auch für Analyse und Traversierung benutzt.

-}

tests :: TestTree
tests =
  testGroup "Tree Tests"

    [
      testCase "wandelt ein konstantes Monom in einen Baum um" $
        monomToExprTree (M 5 0) @?= TConst 5

    , testCase "wandelt ein Monom mit Koeffizient 1 in eine Variable um" $
        monomToExprTree (M 1 3) @?= TVar 3

    , testCase "wandelt ein normales Monom in eine Multiplikation um" $
        monomToExprTree (M 3 2) @?= TMul (TConst 3) (TVar 2)

    , testCase "wandelt ein Nullmonom in eine Konstante 0 um" $
        monomToExprTree (M 0 4) @?= TConst 0

    , testCase "wandelt ein leeres Polynom in eine Konstante 0 um" $
        polyToExprTree (P []) @?= TConst 0

    , testCase "wandelt ein Polynom mit mehreren Monomen in Additionen um" $
        polyToExprTree (P [M 3 2, M 2 1, M 1 0]) @?=
          TAdd (TMul (TConst 3) (TVar 2))
               (TAdd (TMul (TConst 2) (TVar 1))
                     (TConst 1))

    , testCase "gibt die Kindknoten eines Additionsknotens zurück" $
        treeChildren (TAdd (TConst 1) (TVar 1)) @?= [TConst 1, TVar 1]

    , testCase "gibt die Beschriftung eines Knotens zurück" $
        treeLabel (TMul (TConst 2) (TVar 1)) @?= "*"

    , testCase "stellt einen Baum lesbar dar" $
        prettyTree (TAdd (TConst 1) (TVar 1)) @?= "+\n|-- 1\n`-- x\n"

    , testCase "markiert einen Knoten im Baum" $
        prettyTreeMarked 2 (TAdd (TConst 1) (TVar 1)) @?= "+\n|-- >> 1 <<\n`-- x\n"

    , testCase "nutzt die Pretty-Instanz für ExprTree" $
        pretty (TVar 2) @?= "x²\n"

    ]
