module Tree where

import Poly
import ParserSimple
import Data.Ratio (numerator, denominator)

{- Hier kommt die Logik für den Ausdrucksbaum rein: -}

{-

Datentyp für den Ausdrucksbaum
TConst: Konstante (Koeffizient), z.B. ist TConst (3 % 1) ein Ausdrucksbaum, der die Konstante 3 repräsentiert. 
TVar: Variable (Variable x + Exponent), z.B ist TVar 2 ein Ausdrucksbaum, der die Variable x^2 repräsentiert.
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

prettyTree :: ExprTree -> String
prettyTree tree = treeLabel tree ++ "\n" ++ prettyChildren "" (treeChildren tree)

{- 

Diese Hilfsfunktion soll die Kindknoten eines Ausdrucksbaums in einer lesbaren Form darstellen.

Wenn die Liste der Kindknoten leer ist, wird ein leerer String zurückgegeben.
Wenn die Liste der Kindknoten genau ein Element enthält, wird dieses Element mit einem "`-- " Präfix dargestellt.
Wenn die Liste der Kindknoten mehr als ein Element enthält, wird das erste Element mit einem "|-- " Präfix dargestellt und 
die restlichen Elemente werden rekursiv mit einem "|   " Präfix dargestellt. 

-}

prettyChildren :: String -> [ExprTree] -> String
prettyChildren prefix [] = ""
prettyChildren prefix [child] =
   prefix ++ "`-- " ++ treeLabel child ++ "\n"
   ++ prettyChildren (prefix ++ "    ") (treeChildren child)
prettyChildren prefix (child:rest) =
   prefix ++ "|-- " ++ treeLabel child ++ "\n"
   ++ prettyChildren (prefix ++ "|   ") (treeChildren child)
   ++ prettyChildren prefix rest

{- Diese Hilfsfunktion holt je nach Knotentyp die Kindknoten eines Ausdrucksbaums. -}

treeChildren :: ExprTree -> [ExprTree]
treeChildren (TConst k) = []
treeChildren (TVar e) = []
treeChildren (TAdd left right) = [left, right]
treeChildren (TMul left right) = [left, right]

{- Diese Hilfsfunktion gibt je nach Knotentyp die Beschriftung eines Ausdrucksbaums zurück. -}

treeLabel :: ExprTree -> String
treeLabel (TConst k) = prettyRational k
treeLabel (TVar e) = prettyVariable e
treeLabel (TAdd left right) = "+"
treeLabel (TMul left right) = "*"

{- 

Diese Hilfsfunktion wandelt eine rationale Zahl in eine lesbare Form um.
demoniator ist der Nenner der rationalen Zahl, numerator ist der Zähler.

Wenn der Nenner 1 ist, wird nur der Zähler als String zurückgegeben (z.B 3 % 1 wird als "3" dargestellt, da 3 % 1 = 3/1 = 3 ist), 
ansonsten wird der Zähler und der Nenner durch einen Bruchstrich getrennt zurückgegeben (z.B 3 % 2 wird als "3/2" dargestellt, da 3 % 2 = 3/2 ist).

-}

prettyRational :: Rational -> String
prettyRational r
   | denominator r == 1 = show (numerator r)
   | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

{- 

Diese Hilfsfunktion stellt eine Varibale (z.B x^2) mithilfe von prettyExponent in einer lesbaren Form dar.
z.B wird die Eingabe 2 als "x²" dargestellt.

-}

prettyVariable :: Int -> String
prettyVariable 1 = "x"
prettyVariable e = "x" ++ prettyExponent e

{- 

Diese Hilfsfunktion soll einen Exponenten in eine lesbare Form umwandeln.
Je nach Exponent wird eine andere Darstellung gewählt, z.B. 2 wird als "²" dargestellt.

-}

prettyExponent :: Int -> String
prettyExponent 0 = ""
prettyExponent 1 = ""
prettyExponent 2 = "²"
prettyExponent 3 = "³"
prettyExponent e = "^" ++ show e

    

