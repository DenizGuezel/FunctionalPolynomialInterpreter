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

-}

data History a = 
    Empty
    | Entry a (History a)
    deriving (Show, Eq)
