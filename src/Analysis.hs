module Analysis where

import Tree

{- Hier kommt die Logik zur Analyse eines Baums hin, z.B. zur Bestimmung der Tiefe oder der Anzahl der Knoten -}

{- 

Diese Funktion zählt die Tiefe eines Baums. Die Tiefe eines Baums ist definiert als die maximale Anzahl von Knoten, 
die von der Wurzel bis zu einem Blattknoten durchlaufen werden müssen.

z.B. für einen Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Tiefe = 3, da der längste Pfad von der Wurzel (a) zu einem Blattknoten (d oder e) drei Knoten umfasst.

-}

countDepth :: ExprTree a -> Int
countDepth tree = countDepthRec tree 0

{- 

Rekursive Hilfsfunktion, die die Tiefe eines Baums zählt mithilfe von Pattern Matching. 
Sie nimmt als Eingabe einen Baum und die aktuelle Tiefe.

Wenn der aktuelle Knoten ein Blattknoten ist (TConst oder TVar), gibt sie die aktuelle Tiefe zurück.
Wenn der aktuelle Knoten ein innerer Knoten ist (TAdd oder TMul), ruft sie sich selbst rekursiv auf die linken und rechten Teilbäume auf, 
wobei die aktuelle Tiefe um 1 erhöht wird, und gibt das Maximum der beiden Ergebnisse zurück.

-}

countDepthRec :: ExprTree a -> Int -> Int
countDepthRec (TConst _) depth = depth
countDepthRec (TVar _) depth = depth
countDepthRec (TAdd left right) depth = max (countDepthRec left (depth + 1)) (countDepthRec right (depth + 1))
countDepthRec (TMul left right) depth = max (countDepthRec left (depth + 1)) (countDepthRec right (depth + 1))
