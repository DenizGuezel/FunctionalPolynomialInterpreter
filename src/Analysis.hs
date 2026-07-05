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

countDepth :: ExprTree -> Int
countDepth tree = countDepthRec tree 0

{- 

Rekursive Hilfsfunktion, die die Tiefe eines Baums zählt mithilfe von Pattern Matching. 
Sie nimmt als Eingabe einen Baum und die aktuelle Tiefe.

Wenn der aktuelle Knoten ein Blattknoten ist (TConst oder TVar), gibt sie die aktuelle Tiefe zurück.
Wenn der aktuelle Knoten ein innerer Knoten ist (TAdd oder TMul), ruft sie sich selbst rekursiv auf die linken und rechten Teilbäume auf, 
wobei die aktuelle Tiefe um 1 erhöht wird, und gibt das Maximum der beiden Ergebnisse zurück.

-}

countDepthRec :: ExprTree -> Int -> Int
countDepthRec (TConst _) depth = depth
countDepthRec (TVar _) depth = depth
countDepthRec (TAdd left right) depth = max (countDepthRec left (depth + 1)) (countDepthRec right (depth + 1))
countDepthRec (TMul left right) depth = max (countDepthRec left (depth + 1)) (countDepthRec right (depth + 1))


{- 

Diese Funktion zählt die Anzahl der Knoten in einem Baum.

z.B. für einen Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Anzahl der Knoten = 5, da es insgesamt fünf Knoten (a, b, c, d, e) gibt.

-}

countNodes :: ExprTree -> Int
countNodes tree = countNodesRec tree 0

{- 

Rekursive Hilfsfunktion, die die Anzahl der Knoten in einem Baum zählt mithilfe von Pattern Matching.
Sie nimmt als Eingabe einen Baum und die aktuelle Anzahl der Knoten.

Wenn der aktuelle Knoten ein Blattknoten ist (TConst oder TVar), erhöht sie die aktuelle Anzahl der Knoten um 1 und gibt sie zurück.
Wenn der aktuelle Knoten ein innerer Knoten ist (TAdd oder TMul), wird die Funktion zuerst rekursiv auf den rechten Teilbaum aufgerufen, 
wobei die aktuelle Anzahl der Knoten um 1 erhöht wird, und dann auf den linken Teilbaum aufgerufen, 
wobei die aktuelle Anzahl der Knoten um das Ergebnis des rechten Teilbaums erhöht wird.

Es wird so tief wie möglich nach rechts gegangen, bevor nach links gegangen wird, um die Anzahl der Knoten zu zählen.
Quasi sobald ein Blatt erreicht wird, wird zurückgegangen und der linke von jedem durchgegangen rechten Teilbaum wird gezählt.

Sobald ,vom Root aus gesehen, der rechte Teilbaum vollständig gezählt wurde, wird der linke Teilbaum gezählt und die Ergebnisse werden zusammengezählt.

-}

countNodesRec :: ExprTree -> Int -> Int
countNodesRec (TConst _) count = count + 1
countNodesRec (TVar _) count = count + 1
countNodesRec (TAdd left right) count = countNodesRec left (countNodesRec right (count + 1))
countNodesRec (TMul left right) count = countNodesRec left (countNodesRec right (count + 1))
