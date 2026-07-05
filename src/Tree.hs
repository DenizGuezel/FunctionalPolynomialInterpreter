module Tree where

import Poly
import GUI
import ParserSimple

{- Hier kommt die Logik für den Ausdrucksbaum rein: -}

{-

Datentyp für den Ausdrucksbaum
TConst: Konstante, z.B. ist TConst (3 % 1) ein Ausdrucksbaum, der die Konstante 3 repräsentiert. 
TVar: Variable, z.B ist TVar 2 ein Ausdrucksbaum, der die Variable x^2 repräsentiert.
TAdd: Addition, z.B. ist TAdd (TVar 2) (TConst (3 % 1)) ein Ausdrucksbaum, der die Addition von x^2 und 3 repräsentiert.
TMul: Multiplikation, z.B. ist TMul (TVar 2) (TConst (3 % 1)) ein Ausdrucksbaum, der die Multiplikation von x^2 und 3 repräsentiert.

-}

data ExprTree
   = TConst Rational
   | TVar Int
   | TAdd ExprTree ExprTree
   | TMul ExprTree ExprTree
   deriving (Show, Eq)