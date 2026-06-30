module Main where

import Poly

{-

Wir schreiben eine main-Funktion, die Etwas ausführt, sobald man "main" im ghci eintippt.
In diesem Fall ist es sinnvoll, um bestimmte Polynomfunktionen aus Poly.hs auszuführen.

putStrLn ist wie System.out.println(...)

-}

main :: IO()
main = do putStrLn "Test"