{-# LANGUAGE TemplateHaskell #-} -- Ist notwendig, damit wir die $(...) Syntax verwenden können, die wir für die Template Haskell Funktionen benötigen.

module Template where

import Poly
import Language.Haskell.TH

{- Dieses Modul dient dazu, die Funktionen zu definieren, die zur Compile-Zeit Haskell-Code erzeugen-}

{- 

Diese Funktion erzeugt einen Ausdruck, der ein Polynom darstellt, das aus einer Liste von Monomen besteht. 

Sie nimmt eine Liste von Tupeln [(k, e)] als Eingabe.

Jedes Monom wird durch ein Tupel (k, e) repräsentiert, wobei k der Koeffizient und e der Exponent ist. 
Die Funktion verwendet Template Haskell, um den entsprechenden Haskell-Code zur Compile-Zeit zu generieren.

Q Exp ist der Typ für Template Haskell-Ausdrücke (Aktion die einen Haskell-Ausdruck erzeugt), 
und die Funktion gibt einen Ausdruck zurück, der ein Polynom darstellt.

Diese Funktion erstellt zwar ein Polynom, aber nicht direkt als Wert, sondern als Haskell-Code, der später ein Polynom ergibt.
Es läuft nicht zur Laufzeit, sondern zur Compile-Zeit, was bedeutet, dass der erzeugte Code bereits beim Kompilieren des Programms verfügbar ist.

Vorteil ist, dass der erzeugte Code effizienter ist, da er bereits zur Compile-Zeit generiert wird und nicht zur Laufzeit interpretiert werden muss.

-}

polyExp :: [(Integer, Int)] -> Q Exp
polyExp monoms = [| P [M (fromInteger k) e | (k, e) <- monoms] |]

{- 

Diese Funktion erzeugt einen Ausdruck, der ein Polynom mit einem Namen darstellt.

Sie nimmt einen String (Polynomname) und eine Liste von Tupeln [(k, e)] (Monome) als Eingabe.

Sie erstellt ebenfalls wie polyExp zur Compile-Zeit einen Polynom-Ausdruck, 
aber zusätzlich wird der Name des Polynoms als String zurückgegeben.

mit $(polyExp monoms) wird dieser Code an der Stelle eingefügt, an der die Funktion aufgerufen wird, 
und erzeugt den entsprechenden Haskell-Code für das Polynom.

-}

namedPolyExp :: String -> [(Integer, Int)] -> Q Exp
namedPolyExp name monoms = [| (name, $(polyExp monoms)) |]
