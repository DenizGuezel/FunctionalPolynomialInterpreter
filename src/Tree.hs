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

{- 

Diese Hilfsfunktion wird rekursiv aufgerufen, um den Ausdrucksbaum in einen String umzuwandeln.
Sie bekommt als Eingabe einen Ausdrucksbaum und eine Ebene (level), die angibt, wie tief wir uns im Baum befinden.

replicate 4 ' ' erzeugt einen String, der aus 4 Leerzeichen besteht, also "    ". 

Wenn als Parameter ein Baum übergeben wird, der eine Konstante besitzt, sprich nur einen Knoten, welcher der Root ist, 
der aus einer Konstante besteht (z.B 4), dann würde replicate (0 * 2) ' ' ++ show 4 ++ "\n" ausgeführt werden, was "4\n" zurückgibt.
Das Level ist hier 0, da wir uns auf der obersten Ebene befinden (Root).

Wenn als Parameter ein Baum übergeben wird, der eine Variable besitzt, sprich nur einen Knoten, welcher der Root ist,
der aus einer Variable besteht (z.B x^2), dann würde replicate (0 * 2) ' ' ++ "x^" ++ show 2 ++ "\n" ausgeführt werden, was "x^2\n" zurückgibt.

Wenn als Parameter ein Baum übergeben wird, der eine Addition besitzt, sprich nur einen Knoten, welcher der Root ist,
der aus einer Addition besteht (z.B x^2 + 3), dann würde replicate (0 * 2) ' ' ++ "+\n" ++ prettyPrintExprTreeRec left (0 + 1) ++ prettyPrintExprTreeRec right (0 + 1) 
ausgeführt werden, was "+\n  x^2\n  3\n" zurückgibt.

Wenn als Parameter ein Baum übergeben wird, der eine Multiplikation besitzt, sprich nur einen Knoten, welcher der Root ist,
der aus einer Multiplikation besteht (z.B x^2 * 3), dann würde replicate (0 * 2) ' ' ++ "*\n" ++ prettyPrintExprTreeRec left (0 + 1) ++ prettyPrintExprTreeRec right (0 + 1)
ausgeführt werden, was "*\n  x^2\n  3\n" zurückgibt.

-}

prettyPrintExprTreeRec :: ExprTree -> Int -> String
prettyPrintExprTreeRec (TConst k) level = replicate (level * 2) ' ' ++ show k ++ "\n"
prettyPrintExprTreeRec (TVar e) level = replicate (level * 2) ' ' ++ "x^" ++ show e ++ "\n"
prettyPrintExprTreeRec (TAdd left right) level = 
    replicate (level * 2) ' ' ++ "+\n" ++ prettyPrintExprTreeRec left (level + 1) ++ prettyPrintExprTreeRec right (level + 1)
prettyPrintExprTreeRec (TMul left right) level = 
    replicate (level * 2) ' ' ++ "*\n" ++ prettyPrintExprTreeRec left (level + 1) ++ prettyPrintExprTreeRec right (level + 1)
    

