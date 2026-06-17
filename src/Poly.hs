
module Poly where

import Data.Ratio
import Data.List

{-
Ein Monom ist ein einzelner Term, z.b 3x^2 oder 5x
Das Rational ist hier der Koeffizient (z.B 3 oder 5) und das Int der Exponent (also z.b ^2)
Bei dem Rational ist hier das Einsetzen einer Ganzzahl auch möglich, Haskell interpretiert das automatisch.
Ein Bruch wäre z.B 3 % 5 = 3/5 tel 

deriving (Show, Eq) erzeugt automatisch Standardfunktionen.
Show: Damit kann Haskell den Datentyp als Text anzeigen. Beispiel: M 3 2 wird in GHCi angezeigt als: M 3 2
Ohne Show könntest du den Wert nicht einfach ausgeben.
Eq: Damit kann man vergleichen: M 3 2 == M 3 2 ergibt: True, Ohne Eq gäbe es keinen ==-Vergleich.
M ist der Konstruktor, mit welchem man sagt, dass es sich um ein Monom handelt.

data kann mehrere Konstruktoren, auch Parameter, haben und mehrere Werte speichern. Rational und Int sind Parameter

-}

data Monom = M Rational Int
 deriving (Show,Eq)




{-
Der neu definierte Datentyp Poly ist eine Liste von Monomen, also z.b im Code wäre es als Bsp sowas:
P [M 2 3, M 3 4, M 2 1], mathematisch würde es so aussehen: [2x^3, 3x^4,2x^1]
P ist der Konstruktor, was einen richtiges Poly erzeugt (siehe eine Zeile davor Bsp.). Ohne das P, also nur [M 2 3, M 3 4, M 2 1]
wäre es lediglich eine Monomliste, aber mit P sagen wir nochmal, dass diese Liste als Polynom behandelt werden soll

newtype kann nur einen Konstruktor, auch nur einen Parameter haben, und auch genau nur einen Wert speichern.
-}

newtype Poly = P [Monom] 
 deriving (Show,Eq)

{-

Infix ermöglicht es, anstatt der Schreibweise (#^) 3 2, die Schreibweise 3 #^ 2 anzuwenden.
Diese Infixoperation erstellt ein Polynom mit genau einem Monom

-}

infix 8 #^ 
a #^  b = P [M a b] 


{-

Bsp Anwendung der Infixoperation: 3#^2 macht ganz einfach P [M 3 2]

-}

polyEx = 3#^2