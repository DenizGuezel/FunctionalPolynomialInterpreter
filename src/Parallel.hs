module Parallel where

import Poly
import Control.Parallel.Strategies (parMap, rdeepseq)

{- Dieses Modul dient dazu, mehrere Polynome gleichzeitig zu verarbeiten -}

{- 

Diese Funktion dient dazu, mehrere Polynome zu evaluieren. 
Sie nimmt einen Wert x und eine Liste von Polynomen und gibt eine Liste von Rationalen zurück, 
die die Ergebnisse der Evaluierung jedes Polynoms an der Stelle x enthalten.

-}

evaluateMany :: Rational -> [Poly] -> [Rational]
evaluateMany x polys = map (\poly -> evaluate poly x) polys


{- 

Diese Funktion tut im Wesentlichen dasselbe wie evaluateMany, aber sie nutzt die Parallelität, 
um die Evaluierung der Polynome zu beschleunigen.

-}

evaluateManyParallel :: Rational -> [Poly] -> [Rational]
evaluateManyParallel x polys = parMap rdeepseq (\poly -> evaluate poly x) polys


{- Diese Funktion ist eine Erweiterung von evaluateManyParallel, die es ermöglicht, mehrere Polynome zusammen mit ihren Namen zu evaluieren. -}

evaluateNamedManyParallel :: Rational -> [(String, Poly)] -> [(String, Rational)]
evaluateNamedManyParallel x namedPolys = parMap rdeepseq (\(name, poly) -> (name, evaluate poly x)) namedPolys
