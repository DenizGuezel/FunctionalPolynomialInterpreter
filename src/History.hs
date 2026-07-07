module History where


{- In dieses Modul werden die Historie der Berechnungen gespeichert und verwaltet, damit vergangene Berechnungen angezeigt werden können  -}

{- 

Dieser Datentyp repräsentiert die Historie der Berechnungen. Er kann entweder leer sein / bzw. keinen Eintrag haben (Empty) 
oder einen Eintrag (Entry) enthalten, der ein Ergebnis und die restliche Historie enthält.

z.B Wenn wir eine Historie mit zwei Einträgen haben, sieht das so aus:
Entry (PolyResult "f" (P [M 1 0, M 2 1])) 
 (Entry (ValueResult "g" 3) Empty)

mit drei Einträgen:
Entry (PolyResult "f" (P [M 1 0, M 2 1])) 
 (Entry (ValueResult "g" 3) 
  (Entry (DivResult "h" (P [M 1 0]) (P [M 2 0])) Empty))

Am Ende kommt immer Empty, um das Ende der Historie zu markieren und weil das Empty bei einem neuen Eintrag zu dem 
neuen Eintrag hinzugefügt wird, um die Historie zu erweitern.

-}

data History a = 
    Empty
    | Entry a (History a)
    deriving (Show, Eq)

{- 

Diese Funktion fügt einen neuen Eintrag in die Historie ein.
Sie nimmt als Eingabe z.B ein bereits berechnetes Ergebnis (z.B. PolyResult, ValueResult oder DivResult) und 
die aktuelle Historie und gibt eine neue Historie zurück, die den neuen Eintrag enthält.

Wir brauchen nicht nochmal etxra zu überprüfen, ob wir den input auf eine nicht leere Historie anwenden, da wir das Empty sowieso 
immer am Ende der Historie haben und wir den neuen Eintrag immer an den Anfang der Historie setzen, sodass wir die Historie immer erweitern können.

-}

addHistory :: a -> History a -> History a
addHistory input Empty = Entry input Empty

{- 

Diese Funktion gibt die History als Liste zurück.
Sie nimmt als Eingabe eine Historie und gibt eine Liste von Einträgen zurück, die in der Historie enthalten sind.
Also es folgt, dass der gleiche Datentyp, der in der History verwendet wird, also z.B. PolyResult, dann als gleiche 
Datentypliste zurückgegeben wird (hier dann [PolyResult]).

Wenn die Historie leer ist, wird eine leere Liste zurückgegeben.
Ansonsten (Wenn mindestens eine Histore vorhanden ist) wird der erste Eintrag der Historie in die Liste aufgenommen und die Funktion 
wird rekursiv auf die restliche Historie angewendet, um die restlichen Einträge in die Liste aufzunehmen.

-}

historyToList :: History a -> [a]
historyToList Empty = []
historyToList (Entry input resthistorie) = input : historyToList resthistorie


{- 

Diese Funktion gibt den letzten Eintrag der Historie zurück, welcher hinzugefügt wurde, also quasi der letzte hinzugefügte Eintrag.
Sie nimmt als Eingabe eine Historie und gibt ein Maybe von dem gleichen Datentyp zurück, 
der in der Historie verwendet wird (z.B. wenn History PolyResult eingegeben wird, wird Maybe PolyResult zurückgegeben, 
das sagt Entweder bekommt man den Eintrag oder Nothing).

Wenn die Historie leer ist, wird Nothing zurückgegeben.
Wenn die Historie nicht leer ist, wird der erste Eintrag der Historie zurückgegeben, da wir die Historie immer an 
den Anfang erweitern und somit der erste Eintrag der letzte hinzugefügte Eintrag ist.

z.B bei einer Historie mit zwei Einträgen: 
Entry (PolyResult "f" (P [M 1 0, M 2 1])) 
 (Entry (ValueResult "g" 3) Empty)

gibt lastHistory Entry (PolyResult "f" (P [M 1 0, M 2 1])) 
(Entry (ValueResult "g" 3) Empty) = Just (PolyResult "f" (P [M 1 0, M 2 1])) 

-}

lastHistory :: History a -> Maybe a
lastHistory Empty = Nothing
lastHistory (Entry input Empty) = Just input