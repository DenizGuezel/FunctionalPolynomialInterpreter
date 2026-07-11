{-# LANGUAGE TemplateHaskell #-} -- Ist notwendig, damit wir die $(...) Syntax verwenden können, die wir für die Template Haskell Funktionen benötigen.

module Template where

import Poly
import Language.Haskell.TH

{- Dieses Modul dient dazu, die Funktionen zu definieren, die zur Compile-Zeit Haskell-Code erzeugen-}

{- 

Diese Funktion erzeugt einen Ausdruck, der ein Polynom darstellt, das aus einer Liste von Monomen besteht. 
Jedes Monom wird durch ein Tupel (k, e) repräsentiert, wobei k der Koeffizient und e der Exponent ist. 
Die Funktion verwendet Template Haskell, um den entsprechenden Haskell-Code zur Compile-Zeit zu generieren.

-}

polyExp :: [(Integer, Int)] -> Q Exp
polyExp monoms = [| P [M (fromInteger k) e | (k, e) <- monoms] |]
