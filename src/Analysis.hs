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

Sobald , vom Root aus gesehen, der rechte Teilbaum vollständig gezählt wurde, wird der linke Teilbaum gezählt und die Ergebnisse werden zusammengezählt.

-}

countNodesRec :: ExprTree -> Int -> Int
countNodesRec (TConst _) count = count + 1
countNodesRec (TVar _) count = count + 1
countNodesRec (TAdd left right) count = countNodesRec left (countNodesRec right (count + 1))
countNodesRec (TMul left right) count = countNodesRec left (countNodesRec right (count + 1))


{- 

Diese Funktion zählt die Anzahl der Blätter in einem Baum mithilfe der rekursiven Hilfsfunktion countLeavesRec.

z.B für einen Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Anzahl der Blätter = 3, da es insgesamt drei Blätter (d, e, c) gibt.

-}

countLeaves :: ExprTree -> Int
countLeaves tree = countLeavesRec tree 0


{- 

Rekursive Hilfsfunktion, die die Anzahl der Blätter in einem Baum zählt mithilfe von Pattern Matching.
Sie nimmt als Eingabe einen Baum und die aktuelle Anzahl der Blätter.

Wenn der aktuelle Knoten ein Blattknoten ist (TConst oder TVar), erhöht sie die aktuelle Anzahl der Blätter um 1 und gibt sie zurück.
Wenn der aktuelle Knoten ein innerer Knoten ist (TAdd oder TMul), wird die Funktion zuerst rekursiv auf den rechten Teilbaum aufgerufen,

Es wird so tief wie möglich nach rechts gegangen, bevor nach links gegangen wird, um die Anzahl der Blätter zu zählen.
Quasi sobald ein Blatt erreicht wird, wird zurückgegangen und der linke von jedem durchgegangen rechten Teilbaum wird gezählt.

Es können nur maximal zwei Blätter pro innerem Knoten gezählt werden, da jeder innere Knoten genau zwei Kinder hat.

Die Anzahl wird erst erhöht, wenn ein Blattknoten (TConst oder TVar) erreicht wird, und nicht bei inneren Knoten (TAdd oder TMul).

-}

countLeavesRec :: ExprTree -> Int -> Int
countLeavesRec (TConst _) count = count + 1
countLeavesRec (TVar _) count = count + 1
countLeavesRec (TAdd left right) count = countLeavesRec left (countLeavesRec right count)
countLeavesRec (TMul left right) count = countLeavesRec left (countLeavesRec right count)

{- 

Diese Funktion zählt die Anzahl der Operatoren in einem Baum mithilfe der rekursiven Hilfsfunktion countOperatorsRec.

z.B für einen Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Anzahl der Operatoren = 2, da es insgesamt zwei Operatoren (a, b) gibt.
genauer gesagt: (TAdd a (TVar b) (TVar c)) und (TAdd b (TVar d) (TVar e)) sind die beiden Operatoren.

-}

countOperators :: ExprTree -> Int
countOperators tree = countOperatorsRec tree 0

{- 

Rekursive Hilfsfunktion, die die Anzahl der Operatoren in einem Baum zählt mithilfe von Pattern Matching.

Gleiches Vorgehen wie bei countNodesRec, nur dass hier nicht automatisch die Anzahl um 1 erhöht wird, 
wenn wir einen TAdd oder TMul Knoten erreichen, sondern erst, wenn wir die beiden Kinder gezählt haben.

-}

countOperatorsRec :: ExprTree -> Int -> Int
countOperatorsRec (TConst _) count = count
countOperatorsRec (TVar _) count = count
countOperatorsRec (TAdd left right) count = countOperatorsRec left (countOperatorsRec right (count + 1))
countOperatorsRec (TMul left right) count = countOperatorsRec left (countOperatorsRec right (count + 1))

