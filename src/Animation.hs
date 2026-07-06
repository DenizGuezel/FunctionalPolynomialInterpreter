module Animation where


{- 

Hier kommt nicht die echte GUI-Animation rein, sondern die Logik fÃ¼r die Animationsschritte.

Die GUI braucht spÃ¤ter einzelne Schritte, damit sie bei jedem Timer-Tick anzeigen kann,
welcher Knoten gerade besucht wird.

Deshalb bauen wir aus einer normalen Traversierungsliste eine Liste von TraversalStep.

-}

{- Datentyp, der einen einzelnen Schritt in der Animation beschreibt.-}

data TraversalStep = TraversalStep Int String [String]
   deriving (Show, Eq)