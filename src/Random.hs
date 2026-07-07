module Random where

import System.Random (randomRIO)
import Poly

{- Dieses Modul soll die Logik für das zufällige Erzeugen von Polynomen enthalten. Sinnvoll ist es für eine Demonstration der Funktionalität. -}

{- 

Diese Funktion erzeugt ein zufälliges Monom mit einem Koeffizienten im Bereich von -5 bis 5 und einem Exponenten im Bereich von 0 bis 5.
Es wird die Funktion randomRIO aus der System.Random Bibliothek verwendet, um die Zufallszahlen zu generieren. 
Die Funktion gibt ein Monom zurück, das aus dem zufällig gewählten Koeffizienten und Exponenten besteht.

-}

randomMonom :: IO Monom
randomMonom = do
    coeff <- randomRIO (-5, 5)
    exp <- randomRIO (0, 5)
    return (M (fromIntegral coeff) exp)