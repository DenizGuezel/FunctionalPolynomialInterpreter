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
Mit deletePoly stellen wir sicher, dass wir keine Duplikate in der PolyLibrary haben, indem wir das Polynom mit dem gleichen Namen vorher löschen.

-}

savePoly :: PolyName -> Poly -> PolyLibrary -> PolyLibrary
savePoly name poly library = (name,poly) : deletePoly name library

{- 

Diese Funktion sucht nach einem Polynom anhand des Namens in der PolyLibrary. 
Sie nimmt als Eingabe einen Namen und eine PolyLibrary und gibt ein Maybe Poly zurück,

lookup ist eine Funktion aus dem Prelude, die ein Paar (Name, Polynom) in der PolyLibrary sucht und das Polynom zurückgibt, 
wenn es gefunden wird (Just poly), ansonsten gibt sie Nothing zurück.

-}

lookupPoly :: PolyName -> PolyLibrary -> Maybe Poly
lookupPoly name library = lookup name library


{- 

Diese Funktion löscht ein Polynom anhand des Namens aus der PolyLibrary.
Sie nimmt als Eingabe einen Namen und eine PolyLibrary und gibt eine neue PolyLibrary zurück, 
die das Polynom mit dem gegebenen Namen nicht mehr enthält.

Wenn die PolyLibrary leer ist, gibt sie eine leere Liste zurück.
Wenn die PolyLibrary mindestens ein Paar (Name, Polynom) enthält, überprüft sie, 
ob der Name des ersten Paares mit dem gegebenen Namen übereinstimmt. wenn ja, wird das erste Paar entfernt und die restliche PolyLibrary zurückgegeben.
Wenn nein, wird das erste Paar beibehalten und die Funktion wird rekursiv auf die restliche PolyLibrary angewendet, um das Polynom zu löschen.

-}

deletePoly :: PolyName -> PolyLibrary -> PolyLibrary
deletePoly name [] = []
deletePoly name ((n, p):xs)
   | name == n = xs
   | otherwise = (n, p) : deletePoly name xs 

{- 

Diese Funktion listet alle Namen der gespeicherten Polynome in der PolyLibrary auf.
Sie nimmt als Eingabe eine PolyLibrary und gibt eine Liste von PolyName (Namen) zurück, 
die alle Namen der gespeicherten Polynome enthält.

fst ist eine Funktion aus dem Prelude, die das erste Element eines Paares zurückgibt, also den Namen des Polynoms.
z.B bei library =[ ("p1", poly1), ("p2", poly2), ("p3", poly3)] nimmt fst das erste Element jedes Paares, also gibt ["p1", "p2", "p3"] zurück.

-}

listPolys :: PolyLibrary -> [PolyName]
listPolys library = map fst library


{-

Diese Funktion holt aus der gesamten Polynomliste genau die Polynome heraus, die aktuell ausgewählt sind.
selectedNames enthält nur die Namen der ausgewählten Polynome.
Die Funktion filtert dann die PolyLibrary nach diesen Namen.

Das Ergebnis benutzen die Operationen wie Addieren, Subtrahieren, Multiplizieren, Dividieren, Ableiten und Auswerten.

-}

selectedPolys :: PolyLibrary -> [String] -> PolyLibrary
selectedPolys library selectedNames =
   filter (\(name, _) -> name `elem` selectedNames) library
