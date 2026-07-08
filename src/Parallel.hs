module Parallel where

import Poly

{- Dieses Modul dient dazu, mehrere Polynome gleichzeitig zu verarbeiten -}

{- 

Diese Funktion dient dazu, mehrere Polynome gleichzeitig zu evaluieren. 
Sie nimmt einen Wert x und eine Liste von Polynomen und gibt eine Liste von Rationalen zurück, 
die die Ergebnisse der Evaluierung jedes Polynoms an der Stelle x enthalten.

-}

evaluateMany :: Rational -> [Poly] -> [Rational]
evaluateMany x polys = map (\poly -> evaluate poly x) polys