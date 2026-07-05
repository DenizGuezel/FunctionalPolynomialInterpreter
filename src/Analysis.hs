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

countDepth :: Tree a -> Int
countDepth 