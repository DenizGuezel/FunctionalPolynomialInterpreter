module Library where

import Poly

{- Dieses Modul dient dazu, anders wie History, bewusst Polynome mit Namen zu speichern, damit der Benutzer sie später wiederverwenden kann. -}


{- Neuer Datentyp für den Namen eines Polynoms, wir verwenden type, weil wir einen neuen Namen definieren für einen vorhandenen Datentyp -}
type PolyName = String

{- Neuer Datentyp für ein gespeichertes Polynom, das einen Namen und ein Polynom enthält, auch hier type, weil wir einen neuen Namen definieren für einen vorhandenen Datentyp -}
type PolyLibrary = [(PolyName, Poly)]
