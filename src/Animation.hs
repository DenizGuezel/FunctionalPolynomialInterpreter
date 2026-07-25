module Animation where

import Format

{- 

Hier kommt nicht die echte GUI-Animation rein, sondern die Logik für die Animationsschritte.

Die GUI braucht später einzelne Schritte, damit sie bei jedem Timer-Tick anzeigen kann,
welcher Knoten gerade besucht wird.

Deshalb bauen wir aus einer normalen Traversierungsliste eine Liste von TraversalStep.

-}

{- 

Datentyp, der einen einzelnen Schritt in der Animation beschreibt.

Der erste Wert ist die Schrittnummer (z.B. 1. Schritt, 2. Schritt, 3. Schritt).
Der zweite Wert ist der aktuelle Knoten (z.B. "A", "B", "C").
Der dritte Wert ist die Liste der bereits besuchten Knoten (z.B. ["A", "B"]).

-}

data TraversalStep = TraversalStep Int String [String]
   deriving (Show, Eq)

{- 

Diese Funktion bekommt eine Traversierungsliste, z.B. preorder, inorder oder postorder.
Aus jedem Eintrag wird ein Animationsschritt gebaut.

-}

makeTraversalSteps :: [String] -> [TraversalStep]
makeTraversalSteps xs = makeTraversalStepsRec xs [] 1

{- 

Rekursive Hilfsfunktion, die die Traversierungsliste durchgeht und aus jedem Eintrag einen Animationsschritt baut.

Der erste Parameter ist die Traversierungsliste, der zweite Parameter ist die Liste der bereits besuchten Knoten und der dritte Parameter ist die aktuelle Schrittnummer.

Wenn die Traversierungsliste leer ist, wird eine leere Liste zurückgegeben.
Wenn die Traversierungsliste nicht leer ist, wird ein TraversalStep gebaut, der die aktuelle Schrittnummer, den aktuellen Knoten x und die Liste der bereits besuchten Knoten visited enthält.
Danach wird die Funktion rekursiv auf die Restliste aufgerufen, wobei der aktuelle Knoten x zu der Liste der bereits besuchten Knoten hinzugefügt wird und die Schrittnummer um 1 erhöht wird.

-}

makeTraversalStepsRec :: [String] -> [String] -> Int -> [TraversalStep]
makeTraversalStepsRec [] _ _ = []
makeTraversalStepsRec (x:xs) visited stepnumber = TraversalStep stepnumber x visited : makeTraversalStepsRec xs (visited ++ [x]) (stepnumber + 1) 

{- Diese Funktion dient zur Anzeige der Animationsschritte in der GUI. -}

showTraversalStep :: TraversalStep -> String
showTraversalStep (TraversalStep _ current visited) =
   "Traversierung: " ++ formatVisitedSteps visited ++ "\n" ++
   "Aktueller Knoten: " ++ current 
