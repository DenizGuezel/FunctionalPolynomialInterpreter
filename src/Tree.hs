module Tree where

import Poly
import Format

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
monomToExprTree (M 0 _) = TConst 0
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

{- Diese Hilfsfunktion holt je nach Knotentyp die Kindknoten eines Ausdrucksbaums. -}

treeChildren :: ExprTree -> [ExprTree]
treeChildren (TConst _) = []
treeChildren (TVar _) = []
treeChildren (TAdd left right) = [left, right]
treeChildren (TMul left right) = [left, right]

{- Diese Hilfsfunktion gibt je nach Knotentyp die Beschriftung eines Ausdrucksbaums zurück. -}

treeLabel :: ExprTree -> String
treeLabel (TConst k) = prettyRational k
treeLabel (TVar e) = prettyVariable e
treeLabel (TAdd _ _) = "+"
treeLabel (TMul _ _) = "*"

{- 

Diese Funktion stellt einen Ausdrucksbaum als lesbaren Textbaum dar.

Sie benutzt die generische Baumformatierung aus Format.hs.
treeLabel bestimmt, wie ein Knoten angezeigt wird.
treeChildren bestimmt, welche Kindknoten ein Ausdrucksbaum besitzt.

-}

prettyTree :: ExprTree -> String
prettyTree = prettyTreeWith treeLabel treeChildren

{-

Diese Instanz sagt, wie ein ExprTree mit der allgemeinen Pretty-Typklasse dargestellt wird.

Der Ausdrucksbaum bleibt weiterhin im Modul Tree definiert.
Die allgemeine Idee von pretty kommt aber aus Format.hs.
Dadurch kann man später in anderen Modulen einfach pretty tree schreiben, ohne die konkrete Baumfunktion kennen zu müssen.

-}

instance Pretty ExprTree where
   pretty :: ExprTree -> String
   pretty = prettyTree

{- 

Diese Funktion gibt einen Ausdrucksbaum als String dar und markiert den Knoten, der gerade besucht wird.
Mithilfe der generischen Baumformatierung aus Format.hs wird der Ausdrucksbaum dargestellt.

-}

prettyTreeMarked :: Int -> ExprTree -> String
prettyTreeMarked = prettyTreeMarkedWith treeLabel treeChildren
