{-# LANGUAGE TemplateHaskell #-} -- Ist notwendig, damit wir Template-Haskell-Ausdrücke wie Q Exp, conE, appE und $(...) verwenden können.

module Template where

import Language.Haskell.TH
import Poly

{- Dieses Modul dient dazu, die Funktionen zu definieren, die zur Compile-Zeit Haskell-Code erzeugen. -}

{-

Diese Hilfsfunktion erzeugt zur Compile-Zeit den Haskell-Code für ein einzelnes Monom.

Sie bekommt als Eingabe ein Tupel (k, e).
k ist der Koeffizient als Integer.
e ist der Exponent als Int.

Wichtig:
Diese Funktion gibt kein Monom direkt zurück.
Sie gibt Q Exp zurück, also eine Template-Haskell-Aktion, die Haskell-Code für ein Monom erzeugt.

Aus dem Tupel (3, 2) wird ungefähr folgender Haskell-Code erzeugt:
M (fromInteger 3) 2

conE 'M steht für den Konstruktor M.
appE wendet eine Funktion oder einen Konstruktor auf ein Argument an.
litE erzeugt einen Literal-Ausdruck, also z.B. die Zahl 3 oder 2 im erzeugten Code.

-}

monomExp :: (Integer, Int) -> Q Exp
monomExp (k, e) =
   appE
      (appE (conE 'M) (appE (varE 'fromInteger) (litE (integerL k))))
      (litE (integerL (fromIntegral e)))

{- 

Diese Funktion erzeugt einen Ausdruck, der ein Polynom darstellt, das aus einer Liste von Monomen besteht. 

Sie nimmt eine Liste von Tupeln [(k, e)] als Eingabe.

Jedes Monom wird durch ein Tupel (k, e) repräsentiert, wobei k der Koeffizient und e der Exponent ist. 
Die Funktion verwendet Template Haskell, um den entsprechenden Haskell-Code zur Compile-Zeit zu generieren.

Q Exp ist der Typ für Template-Haskell-Ausdrücke.
Das bedeutet: Die Funktion gibt nicht direkt ein Poly zurück, sondern Haskell-Code, der später ein Poly ergibt.

Aus dieser Eingabe:
[(3,2),(2,1),(1,0)]

wird beim Kompilieren ungefähr dieser Code erzeugt:
P [M (fromInteger 3) 2, M (fromInteger 2) 1, M (fromInteger 1) 0]

Der Vorteil ist, dass feste Beispielpolynome nicht erst zur Laufzeit interpretiert werden müssen,
sondern schon zur Compile-Zeit als echter Haskell-Ausdruck erzeugt werden.

-}

polyExp :: [(Integer, Int)] -> Q Exp
polyExp monoms = appE (conE 'P) (listE (map monomExp monoms))

{- 

Diese Funktion erzeugt einen Ausdruck, der ein benanntes Polynom darstellt.

Sie nimmt einen String (Polynomname) und eine Liste von Tupeln [(k, e)] (Monome) als Eingabe.

polyExp erzeugt nur den Code für ein Poly.
namedPolyExp erzeugt zusätzlich ein Tupel aus Name und Polynom.

Das passt genau zum Datentyp PolyLibrary:
type PolyLibrary = [(PolyName, Poly)]

Ein Eintrag in dieser Liste hat also die Form:
(String, Poly)

Aus:
namedPolyExp "b1" [(3,2),(2,1),(1,0)]

wird beim Kompilieren ungefähr:
("b1", P [M (fromInteger 3) 2, M (fromInteger 2) 1, M (fromInteger 1) 0])

-}

namedPolyExp :: String -> [(Integer, Int)] -> Q Exp
namedPolyExp name monoms = tupE [litE (stringL name), polyExp monoms]