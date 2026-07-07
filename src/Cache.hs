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

Es wird nicht das Ergbenis einer Operation gespeichert, sondern die Operation selbst, welche ausgeführt werden soll.

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

Bei diesem Datentyp arbeiten wir mit type, da wir nur einen neuen Namen für bereits existierende 
Datentypen erstellen wollen und keine neuen Konstruktoren oder Funktionen benötigen.

-}

type Cache = [(Operation, String)]

{- 

Diese Funktion sucht nach einer Operation in der Cache und gibt den zugehörigen Namen zurück.

Sie nimmt als Eingabe eine Operation und eine Cache (Liste von Operationen und Namen) und gibt, wenn die Operation gefunden wurde, 
den zugehörigen Namen zurück, wenn nicht, dann Nothing.

Wenn der Cache leer ist, wird Nothing zurückgegeben. 
Wenn ein Cache mit mindestens einer Operation vorhanden ist, wird die erste Operation überprüft und wenn sie mit der gesuchten Operation 
übereinstimmt, wird der zugehörige Name zurückgegeben. Wenn der erste Eintrag nicht mit der gesuchten Operation übereinstimmt, 
wird die Funktion rekursiv auf die restliche Cache angewendet, um die restlichen Einträge zu überprüfen.

-}

lookupCache :: Operation -> Cache -> Maybe String
lookupCache op [] = Nothing
lookupCache op ((cachedOp, name):rest) =
    if op == cachedOp
        then Just name
        else lookupCache op rest

{- 

Diese Funktion fügt eine neue Operation und ihren zugehörigen Namen zum Cache hinzu.

Sie bekommt als Eingabe eine Operation, einen Namen und eine Cache (Liste von Operationen und Namen, wo es eingefügt werden soll) 
und gibt eine neue Cache zurück, die die neue Operation und den Namen enthält.

-}

insertCache :: Operation -> String -> Cache -> Cache
insertCache op name cache = (op, name) : cache