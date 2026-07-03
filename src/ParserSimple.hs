module ParserSimple where 

{- 

Ziel des ParserSimple ist es aus einem String einen Polynom-Ausdruck zu parsen, 
das ist genau andersrum als die toLatex Funktion, die aus einem Polynom-Ausdruck einen String erzeugt.

-}

import Poly
import Data.Ratio
import Text.Read(readMaybe)


