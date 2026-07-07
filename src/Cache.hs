module Cache where

import Poly

{- 

Dieses Modul dient dazu, bereits berechnete Ergebnisse wiederverwendbar machen zu können. 
Anders wie Library.hs, ist die Cache nicht für den Benutzer sichtbar und speichert auch nicht bewusste Polynome mit Namen,
sondern ist die Cache eher intern für das Programm und speichert berechnete Operationsergebnisse.

Library: "Welche Polynome hat der Benutzer bewusst gespeichert?"
Cache: "Welche Berechnungen hat das Programm durchgeführt?"

-}

{- 

Dieser Datentyp repräsentiert die verschiedenen Operationen, 
die angewendet werden können, um ein Ergebnis zu berechnen.

z.B Normalize P [M 1 0, M 2 1] repräsentiert die Normalisierung des Polynoms P [M 1 0, M 2 1].

-}

data Operation
    = Normalize Poly
    | Negate Poly
    | Derive Poly
    | Evaluate Poly Rational
    | Add Poly Poly
    | Sub Poly Poly
    | Mul Poly Poly
   deriving (Show, Eq)

{- 

Dieser Datentyp repräsentiert den Cache, der verschiedene Operationen als Paar mit einem String speichert. 
z.B [(Normalize (P [M 1 0, M 2 1]), "f"), (Negate (P [M 1 0, M 2 1]), "g")]. 

-}

type Cache = [(Operation, String)]