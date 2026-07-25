module Random where

import System.Random (randomRIO)
import Poly
import Library (PolyName)

{- Dieses Modul soll die Logik für das zufällige Erzeugen von Polynomen enthalten. Sinnvoll ist es für eine Demonstration der Funktionalität. -}

{- 

Diese Funktion erzeugt ein zufälliges Monom mit einem Koeffizienten im Bereich von -5 bis 5 und einem Exponenten im Bereich von 0 bis 5.

Sie bekommt keine Parameter übergeben und gibt ein Monom zurück, welches aus dem do-Block generiert wird.

Es wird die Funktion randomRIO aus der System.Random Bibliothek verwendet, um die Zufallszahlen zu generieren. 
Die Funktion gibt ein Monom zurück, das aus dem zufällig gewählten Koeffizienten und Exponenten besteht.

-}

randomMonom :: IO Monom
randomMonom = do
   coeff <- randomRIO (-5 :: Int, 5)
   expo <- randomRIO (0 :: Int, 5)
   return (M (fromIntegral coeff) expo)

{- 

Diese Funktion erzeugt ein zufälliges Polynom mit einer zufälligen Anzahl von Monomen (zwischen 1 und 5).

Sie bekommt keine Parameter übergeben und gibt ein Polynom zurück, welches aus dem do-Block generiert wird.

Jedes Monom wird durch die numMonoms-male Wiederholung von randomMonom erzeugt. 
Die Monome werden in einer Liste gesammelt und anschließend normalisiert, um das Polynom zu erstellen.

Die Funktion gibt ein Polynom zurück, das aus den zufällig erzeugten Monomen besteht.

-}

randomPoly :: IO Poly
randomPoly = do
   numMonoms <- randomRIO (1 :: Int, 5)
   monoms <- sequence (replicate numMonoms randomMonom)
   return (normalize (P monoms))

{- 

Diese Funktion erzeugt ein zufälliges Polynom mit einem gegebenen Namen.
Sie bekommt den Namen als String übergeben und gibt ein Tupel aus dem Namen und dem zufällig erzeugten Polynom zurück.

-}

randomNamedPoly :: String -> IO (PolyName, Poly)
randomNamedPoly name = do
   poly <- randomPoly
   return (name, poly)