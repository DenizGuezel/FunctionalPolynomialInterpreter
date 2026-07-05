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

Beispielsweise wäre ein Monom M 3 2, das 3x^2 repräsentiert, in einem Ausdrucksbaum als TMul (TConst (3 % 1)) (TVar 2) dargestellt.

-}

data ExprTree
   = TConst Rational
   | TVar Int
   | TAdd ExprTree ExprTree
   | TMul ExprTree ExprTree
   deriving (Show, Eq)


{- 

Diese Funktion soll ein Monom in einen Ausdrucksbaum umwandeln.
Sie bekommt als Eingabe ein Monom, z.B. M 3 2 und gibt als Ausgabe einen Ausdrucksbaum zurück, z.B. TMul (TConst (3 % 1)) (TVar 2).

Mithilfe von pattern matching wird das Monom in seine Bestandteile zerlegt, nämlich den Koeffizienten k und den Exponenten e.

-}

monomToExprTree :: Monom -> ExprTree
monomToExprTree (M k 0) = TConst k
monomToExprTree (M 1 e) = TVar e
monomToExprTree (M 0 e) = TConst 0
monomToExprTree (M k e) = TMul (TConst k) (TVar e)


{-

Diese Funktion soll ein Polynom in einen Ausdrucksbaum umwandeln.
Sie bekommt als Eingabe ein Polynom, z.B. P [M 3 2, M 2 1, M 1 0] und gibt als Ausgabe einen Ausdrucksbaum zurück, 
z.B. TAdd (TAdd (TMul (TConst (3 % 1)) (TVar 2)) (TMul (TConst (2 % 1)) (TVar 1))) (TConst (1 % 1)).

Ebenfalls mithilfe von Pattern matching wird das Polynom in seine Bestandteile zerlegt, nämlich die Liste der Monome.

-}

polyToExprTree :: Poly -> ExprTree
polyToExprTree (P []) = TConst 0
polyToExprTree (P [m]) = monomToExprTree m
polyToExprTree (P (m:ms)) = TAdd (monomToExprTree m) (polyToExprTree (P ms))


{- 

Hauptfunktion, die einen Ausdrucksbaum in einen String umwandelt, 
der den Ausdruck in einer lesbaren Form darstellt.

-}

prettyPrintExprTree :: ExprTree -> String 
prettyPrintExprTree tree = prettyPrintExprTreeRec tree 0



