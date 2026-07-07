module Graph where

import Poly
import Format (prettyRational) 

{- Dieses Modul dient dazu, einen Graphen zu visualisieren, sie enthält die ganze Logik. -}


{-

Diese Funktion berechnet für eine Liste von x-Werten die entsprechenden y-Werte für 
ein gegebenes Polynom und gibt eine Liste von Tupeln zurück, die die Punkte darstellen.

Sie bekommt als Eingabe ein Polynom und eine Liste von x-Werten und gibt als Ausgabe eine Liste von 
Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

-}

samplePoints :: Poly -> [Rational] -> [(Rational, Rational)]
samplePoints poly xs = map (\x -> (x, evaluate poly x)) xs


{- 

Diese Funktion berechnet für ein gegebenes Polynom eine unendliche Liste von Punkten, die auf dem Graphen des Polynoms liegen.
Sie bekommt als Eingabe ein Polynom und gibt als Ausgabe eine unendliche Liste von Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

x ist hierbei eine unendliche Liste von Rationalen Zahlen, die von -10 bis unendlich geht mithilfe von Lazy Evaluation.

Sinn dieser Funktion ist es, die Punkte des Graphen des Polynoms zu berechnen, um sie in der GUI anzuzeigen.

-}

infinitePoints :: Poly -> [(Rational, Rational)]
infinitePoints poly = [(x, evaluate poly x) | x <- [(-10)..]]

{- 

Diese Funktion zeigt die ersten n Punkte des Graphen eines Polynoms an, die auf dem Graphen des Polynoms liegen.
Sie nimmt mithilfe von take n Punkte aus der unendlichen Liste von Punkten, die von der Funktion infinitePoints zurückgegeben wird.

-}

visiblePoints :: Int -> Poly -> [(Rational, Rational)]
visiblePoints n poly = take n (infinitePoints poly)


{- 

Diese Funktion erzeugt einen Standardausschnitt von Punkten für ein gegebenes Polynom, um den Graphen in der GUI anzuzeigen.
Standard ist hier dann [-10 .. 10] für x-Werte und die entsprechenden y-Werte werden berechnet.

-}

defaultPoints :: Poly -> [(Rational, Rational)]
defaultPoints poly = visiblePoints 21 poly 