{-# LANGUAGE TemplateHaskell #-} -- Ist notwendig, damit wir die $(...) Syntax benutzen können.

module Examples where

import Library
import Poly
import Template

{- Dieses Modul verwendet Template.hs, um feste Beispielpolynome zur Compile-Zeit zu erzeugen. -}

{-

Dieses Beispielpolynom wird mit Template Haskell erzeugt.

Der Ausdruck $(polyExp [(3,2),(2,1),(1,0)]) wird nicht zur Laufzeit geparst.
Stattdessen wird polyExp beim Kompilieren ausgeführt und erzeugt daraus echten Haskell-Code vom Typ Poly.

Mathematisch entspricht dieses Polynom: 3x² + 2x + 1

-}

examplePoly :: Poly
examplePoly = $(polyExp [(3,2),(2,1),(1,0)])

{-

Diese Beispielbibliothek wird mit Template Haskell erzeugt.

Jeder Eintrag wird mit namedPolyExp erzeugt.
Das Ergebnis ist jeweils ein Paar aus Name und Polynom, also genau das Format, das PolyLibrary erwartet.

Die Namen beginnen bewusst mit b statt p oder r.
p wird bereits für normal hinzugefügte Polynome verwendet.
r wird bereits für Zufallspolynome verwendet.
Dadurch erkennt man in der GUI sofort, dass diese Polynome aus der Beispielbibliothek kommen.

-}

exampleLibrary :: PolyLibrary
exampleLibrary =
   [ $(namedPolyExp "b1" [(3,2),(2,1),(1,0)])
   , $(namedPolyExp "b2" [(1,1),(-4,0)])
   , $(namedPolyExp "b3" [(5,3)])
   , $(namedPolyExp "b4" [(2,0)])
   ]