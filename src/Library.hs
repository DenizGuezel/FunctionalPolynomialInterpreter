module Library where

import Poly

{- Dieses Modul dient dazu, anders wie History, bewusst Polynome mit Namen zu speichern, damit der Benutzer sie später wiederverwenden kann. -}


{- Neuer Datentyp für den Namen eines Polynoms, wir verwenden type, weil wir einen neuen Namen definieren für einen vorhandenen Datentyp -}
type PolyName = String

{- Neuer Datentyp für ein gespeichertes Polynom, das einen Namen und ein Polynom enthält, auch hier type, weil wir einen neuen Namen definieren für einen vorhandenen Datentyp -}
type PolyLibrary = [(PolyName, Poly)]

{- 

Diese Funktion Speichert ein Polynom als Paar (Name, Polynom) in der einer PolyLibrary ab

Sie nimmt als Eingabe einen Namen, ein Polynom und eine PolyLibrary (auf die wir die Speicherung anwenden) 
und gibt eine neue PolyLibrary zurück, die das neue Paar enthält.

Mit ":" hängen wir das neue Paar (Name, Polynom) an die bestehende PolyLibrary an, um eine neue PolyLibrary zu erstellen.

-}

savePoly :: PolyName -> Poly -> PolyLibrary -> PolyLibrary
savePoly name poly library = (name,poly) : library
