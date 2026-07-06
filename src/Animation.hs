module Animation where

{- 

Hier kommt nicht die echte GUI-Animation rein, sondern die Logik fÃ¼r die Animationsschritte.

Die GUI braucht spÃ¤ter einzelne Schritte, damit sie bei jedem Timer-Tick anzeigen kann,
welcher Knoten gerade besucht wird.

Deshalb bauen wir aus einer normalen Traversierungsliste eine Liste von TraversalStep.

-}

{- 

Datentyp, der einen einzelnen Schritt in der Animation beschreibt.

Der erste Wert ist die Schrittnummer (z.b. 1. Schritt, 2. Schritt, 3. Schritt).
Der zweite Wert ist der aktuelle Knoten (z.b. "A", "B", "C").
Der dritte Wert ist die Liste der bereits besuchten Knoten (z.b. ["A", "B"]).

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
Wenn die Traversierungsliste nicht leer ist, wird ein TraversalSetep gebaut, der die aktuelle Schrittnummer, den aktuellen Knoten x und die Liste der bereits besuchten Knoten visited enthält. 
Danach wird die Funktion rekursiv auf die Restliste aufgerufen, wobei der aktuelle Knoten x zu der Liste der bereits besuchten Knoten hinzugefügt wird und die Schrittnummer um 1 erhöht wird.

-}

makeTraversalStepsRec :: [String] -> [String] -> Int -> [TraversalStep]
makeTraversalStepsRec [] _ _ = []
makeTraversalStepsRec (x:xs) visited stepnumber = TraversalStep stepnumber x visited : makeTraversalStepsRec xs (visited ++ [x]) (stepnumber + 1) 

{- Diese Funktion dient zu der Anzeige der Animationsschritte in der GUI -}

showTraversalStep :: TraversalStep -> String
showTraversalStep (TraversalStep stepnumber current visited) = 
   "Traversierung: " ++ formatVisitedSteps visited ++ "\n" ++
   "Aktueller Knoten: " ++ current 

{- 

Diese Funktion dient dazu, eine Liste von Animationsschritten in einen String umzuwandeln, 
der die einzelnen Schritte in einer lesbaren Form darstellt.

Sie bekommt die Liste der besuchten Knoten übergeben und ruft die rekursive Hilfsfunktion formatVisitedStepsRec auf, 
die die Liste der besuchten Knoten durchgeht und einen String baut, beginnend bei Schritt 1.

Es erzeugt eine Bsp-Ausgabe: formatVisitedSteps ["A", "B", "C"] -> "Schritt: 1, Knoten: A\nSchritt: 2, Knoten: B\nSchritt: 3, Knoten: C\n"

-}

formatVisitedSteps :: [String] -> String
formatVisitedSteps xs = formatVisitedStepsRec xs 1

{- 

Diese rekursive Hilfsfunktion geht die Liste der besuchten Knoten durch und baut einen String, der die einzelnen Schritte in einer lesbaren Form darstellt.
Sie bekommt die Liste der besuchten Knoten und die aktuelle Schrittnummer übergeben.

-}

formatVisitedStepsRec :: [String] -> Int -> String
formatVisitedStepsRec [] _ = ""
formatVisitedStepsRec (x:xs) stepnumber = "Schritt: " ++ show stepnumber ++ ", Knoten: " ++ x ++ "\n" ++ formatVisitedStepsRec xs (stepnumber + 1)
