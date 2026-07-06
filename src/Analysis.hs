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
countDepth tree = countDepthRec tree 1

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

Ein innerer Knoten hat genau zwei Teilbäume. Deshalb wird die Anzahl der Blätter aus dem linken und rechten Teilbaum zusammengerechnet.

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
wenn wir einen TConst oder TVar Knoten erreichen, sondern erst, wenn wir die beiden Kinder gezählt haben.

Quasi wenn wir einen Baum übergeben bekommen, der nur aus einem TConst oder TVar Knoten besteht, wird die Anzahl der Operatoren = 0 sein,
aber wenn wir einen Baum übergeben bekommen, der aus einem TAdd oder TMul Knoten besteht, wird die Anzahl der Operatoren = 1 sein.

TAdd und TMul sind die Operatoren, die wir zählen wollen, und TConst und TVar sind die Kinder dieser Operatoren, die wir nicht zählen wollen.

visualisiert z.B.:

        TAdd / bzw. (+)
       /    \
     TVar   TVar

-}

countOperatorsRec :: ExprTree -> Int -> Int
countOperatorsRec (TConst _) count = count
countOperatorsRec (TVar _) count = count
countOperatorsRec (TAdd left right) count = countOperatorsRec left (countOperatorsRec right (count + 1))
countOperatorsRec (TMul left right) count = countOperatorsRec left (countOperatorsRec right (count + 1))

{-

Diese Funktion zählt die Anzahl der Variablen in einem Baum mithilfe der rekursiven Hilfsfunktion countVariablesRec.

z.B für einen Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Anzahl der Variablen = 3, da es insgesamt drei Variablen (b, d, e) gibt.
genauer gesagt: (TVar b), (TVar d) und (TVar e)

-}

countVariables :: ExprTree -> Int
countVariables tree = countVariablesRec tree 0

{- 

Rekursive Hilfsfunktion, die die Anzahl der Variablen in einem Baum zählt mithilfe von Pattern Matching.

Gleiches Vorgehen wie bei countLeavesRec, nur dass hier nicht automatisch die Anzahl um 1 erhöht wird,
wenn wir einen TConst Knoten erreichen, sondern erst, wenn wir einen TVar Knoten erreichen, denn dieser repräesentiert eine Variable.

Ebenfalls wird die Anzahl nicht erhöht, wenn wir einen TAdd oder TMul Knoten erreichen, da diese keine Variablen sind.

Wir gehen hier ebenfalls so tief wie möglich nach rechts, bevor wir nach links gehen, um die Anzahl der Variablen zu zählen.

-}

countVariablesRec :: ExprTree -> Int -> Int
countVariablesRec (TConst _) count = count
countVariablesRec (TVar _) count = count + 1
countVariablesRec (TAdd left right) count = countVariablesRec left (countVariablesRec right count)
countVariablesRec (TMul left right) count = countVariablesRec left (countVariablesRec right count) 

{- 

Diese Funktion gibt die Knoten eines Baums in Pre-Order Traversal zurück. 
Pre-Order Traversal bedeutet, dass wir zuerst den aktuellen Knoten besuchen, dann den linken Teilbaum und schließlich den rechten Teilbaum.

Wenn als Parameter ein Baum eingegeben wird, der nur aus einem TConst oder TVar Knoten besteht, wird die Zahl als String verschönert zurückgegeben.
Wenn als Parameter ein Baum eingegeben wird, der aus einem TAdd oder TMul Knoten besteht, wird der Operator
 als String zurückgegeben und es wird rekursiv der linke und rechte Teilbaum besucht.

Bsp bei einem Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Pre-Order Traversal = [a, b, d, e, c], da wir zuerst a besuchen, dann b, dann d, dann e und schließlich c.

-}

preOrder :: ExprTree -> [String]
preOrder (TConst n) = [prettyRational n]
preOrder (TVar v) = [prettyVariable v]
preOrder (TAdd left right) = ["+"] ++ preOrder left ++ preOrder right
preOrder (TMul left right) = ["*"] ++ preOrder left ++ preOrder right


{- 

Diese Funktion gibt die Knoten eines Baums in In-Order Traversal zurück. 
In-Order Traversal bedeutet, dass wir zuerst den linken Teilbaum besuchen, dann den aktuellen Knoten und schließlich den rechten Teilbaum.

Wenn als Parameter ein Baum eingegeben wird, der nur aus einem TConst oder TVar Knoten besteht, wird die Zahl als String verschönert zurückgegeben.
Wenn als Parameter ein Baum eingegeben wird, der aus einem TAdd oder TMul Knoten besteht, wird zuerst linke Teilbaum besucht, 
dann der Operator als String zurückgegeben und schließlich der rechte Teilbaum besucht.

Bsp bei einem Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die In-Order Traversal = [d, b, e, a, c], da wir zuerst d besuchen, dann b, dann e, dann a und schließlich c.

-}

inOrder :: ExprTree -> [String]
inOrder (TConst n) = [prettyRational n]
inOrder (TVar v) = [prettyVariable v]
inOrder (TAdd left right ) = inOrder left ++ ["+"] ++ inOrder right
inOrder (TMul left right) = inOrder left ++ ["*"] ++ inOrder right

{- 

Diese Funktion gibt die Knoten eines Baums in Post-Order Traversal zurück. 
Post-Order Traversal bedeutet, dass wir zuerst den linken Teilbaum besuchen, dann den rechten Teilbaum und schließlich den aktuellen Knoten.

Wenn als Parameter ein Baum eingegeben wird, der nur aus einem TConst oder TVar Knoten besteht, wird die Zahl als String verschönert zurückgegeben.
Wenn als Parameter ein Baum eingegeben wird, der aus einem TAdd oder TMul Knoten besteht, wird zuerst linke Teilbaum besucht, 
dann der rechte Teilbaum besucht und schließlich der Operator als String zurückgegeben.

Bsp bei einem Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Post-Order Traversal = [d, e, b, c, a], da wir zuerst d besuchen, dann e, dann b, dann c und schließlich a.

-}

postOrder :: ExprTree -> [String]
postOrder (TConst n) = [prettyRational n]
postOrder (TVar v) = [prettyVariable v]
postOrder (TAdd left right) = postOrder left ++ postOrder right ++ ["+"]
postOrder (TMul left right) = postOrder left ++ postOrder right ++ ["*"]

{- 

Diese Funktion gibt die Knoten eines Baums in Level-Order Traversal zurück mithilfe der rekursiven Hilsfunktion. 
Level-Order Traversal bedeutet, dass wir die Knoten auf jeder Ebene von links nach rechts besuchen.

Bsp bei einem Baum der Form:

        a
       / \
      b   c
     / \
    d   e

gilt die Level-Order Traversal = [a, b, c, d, e], da wir zuerst a besuchen, dann b und c auf der zweiten Ebene und schließlich d und e auf der dritten Ebene.

-}

levelOrder :: ExprTree -> [String]
levelOrder tree = levelOrderRec [tree]

{- 

Wenn der Baum leer ist, wird eine leere Liste zurückgegeben.
Wenn mindestens ein Baum vorhanden ist in der Liste, wird der erste Baum besucht und sein Label zur Ergebnisliste hinzugefügt.

Dann werden die Kindknoten des ersten Baums zur Liste der zu besuchenden Bäume hinzugefügt und die Funktion wird rekursiv aufgerufen,
bis alle Bäume besucht wurden.

-}

levelOrderRec :: [ExprTree] -> [String]
levelOrderRec [] = []
levelOrderRec (tree:rest) = treeLabel tree : levelOrderRec (rest ++ treeChildren tree)

{- Diese Funktion gibt die Knoten (in Form einer String-Liste durch Order-Verwendung) eines Baums in einer kommagetrennten Liste zurück -}

showTraversal :: [String] -> String
showTraversal xs = unwords xs

{- Diese Funktion dient zur Darstellung der Baumanalyse in der GUI -}

analyseTree :: ExprTree -> String
analyseTree tree =
   "Tiefe: " ++ show (countDepth tree) ++ "\n"
   ++ "Knoten: " ++ show (countNodes tree) ++ "\n"
   ++ "Blätter: " ++ show (countLeaves tree) ++ "\n"
   ++ "Operatoren: " ++ show (countOperators tree) ++ "\n"
   ++ "Variablen: " ++ show (countVariables tree) ++ "\n"
   ++ "Pre-Order Traversal: " ++ showTraversal (preOrder tree) ++ "\n"
   ++ "In-Order Traversal: " ++ showTraversal (inOrder tree) ++ "\n"
   ++ "Post-Order Traversal: " ++ showTraversal (postOrder tree) ++ "\n"
   ++ "Level-Order Traversal: " ++ showTraversal (levelOrder tree) ++ "\n" 