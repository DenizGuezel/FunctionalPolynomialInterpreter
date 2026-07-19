module Graph where

import Poly
import Format (Pretty(..))

{- Dieses Modul dient dazu, einen Graphen zu visualisieren, sie enthält die ganze Logik. -}

{- Standardwerte für den sichtbaren Graphbereich und die Wertetabelle. -}

defaultGraphStart :: Rational
defaultGraphStart = -10

defaultGraphStep :: Rational
defaultGraphStep = 1

defaultGraphPointCount :: Int
defaultGraphPointCount = 21

defaultTableStart :: Rational
defaultTableStart = -5

defaultTablePointCount :: Int
defaultTablePointCount = 11

{-

Diese Funktion berechnet für eine Liste von x-Werten die entsprechenden y-Werte für 
ein gegebenes Polynom und gibt eine Liste von Tupeln zurück, die die Punkte darstellen.

Sie bekommt als Eingabe ein Polynom und eine Liste von x-Werten und gibt als Ausgabe eine Liste von 
Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

Intern nutzt sie pointStream.
Dadurch ist die Logik für endliche und unendliche x-Listen dieselbe.

-}

samplePoints :: Poly -> [Rational] -> [(Rational, Rational)]
samplePoints poly xs = pointStream poly xs

{-

Diese Funktion erzeugt eine unendliche Liste von x-Werten.

start ist der erste x-Wert.
step ist die Schrittweite zwischen zwei x-Werten.

Der wichtige Punkt für Lazy Evaluation ist:
Diese Liste ist theoretisch unendlich, aber Haskell berechnet nicht sofort alle Werte.
Erst wenn eine andere Funktion, z.B. take, konkrete Werte verlangt, werden diese Werte wirklich erzeugt.

Beispiel:
xValueStream (-10) 1 -> [-10, -9, -8, -7, ...]

-}

xValueStream :: Rational -> Rational -> [Rational]
xValueStream start step = iterate (+ step) start

{-

Diese Funktion wandelt eine Liste von x-Werten in eine Liste von Punkten um.

Auch diese Funktion ist lazy, weil map in Haskell verzögert arbeitet.
Das bedeutet: Die y-Werte werden nicht alle sofort berechnet, sondern nur dann, wenn sie wirklich gebraucht werden.

Wenn die Eingabe eine unendliche Liste von x-Werten ist, entsteht hier auch eine unendliche Liste von Punkten.
Das ist nur möglich, weil Haskell Lazy Evaluation benutzt.

-}

pointStream :: Poly -> [Rational] -> [(Rational, Rational)]
pointStream poly xs = map (\x -> (x, evaluate poly x)) xs

{- 

Diese Funktion berechnet für ein gegebenes Polynom eine unendliche Liste von Punkten, die auf dem Graphen des Polynoms liegen.
Sie bekommt als Eingabe ein Polynom und gibt als Ausgabe eine unendliche Liste von Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

Die x-Werte kommen aus xValueStream.
Die y-Werte werden mit pointStream berechnet.

Wichtig:
Diese Funktion allein würde eine unendliche Punktliste beschreiben.
Erst visiblePoints oder visiblePointsFrom schneiden daraus einen endlichen Bereich heraus.

-}

infinitePoints :: Poly -> [(Rational, Rational)]
infinitePoints poly = pointStream poly (xValueStream defaultGraphStart defaultGraphStep)

{-

Diese Funktion zeigt die ersten n Punkte eines Polynoms an.

Sie nimmt mithilfe von take n Punkte aus der unendlichen Liste von Punkten, die von infinitePoints zurückgegeben wird.
Dadurch wird Lazy Evaluation praktisch sichtbar:
infinitePoints beschreibt unendlich viele Punkte, aber take n erzwingt nur die ersten n Punkte.

-}

visiblePoints :: Int -> Poly -> [(Rational, Rational)]
visiblePoints n poly = take n (infinitePoints poly)

{-

Diese Funktion ist die allgemeinere Variante von visiblePoints.

Man kann hier zusätzlich festlegen, bei welchem x-Wert gestartet wird und welche Schrittweite verwendet wird.
Dadurch kann die GUI später verschiedene Ausschnitte eines Graphen anzeigen, ohne die Lazy-Evaluation-Logik zu ändern.

Auch hier gilt:
xValueStream erzeugt eine unendliche Liste.
pointStream wandelt sie lazy in Punkte um.
take n fordert nur den sichtbaren Ausschnitt an.

-}

visiblePointsFrom :: Int -> Rational -> Rational -> Poly -> [(Rational, Rational)]
visiblePointsFrom n start step poly = take n (pointStream poly (xValueStream start step))

{- 

Diese Funktion erzeugt einen Standardausschnitt von Punkten für ein gegebenes Polynom, um den Graphen in der GUI anzuzeigen.
Standard ist hier der Bereich von -10 bis 10 bei Schrittweite 1.

Der Bereich entsteht nicht durch eine fertige endliche Liste, sondern durch take auf einer unendlichen Punktliste.
Damit bleibt das Beispiel für Lazy Evaluation erhalten.

-}

defaultPoints :: Poly -> [(Rational, Rational)]
defaultPoints poly = visiblePoints defaultGraphPointCount poly

{-

Diese Funktion erzeugt die Punkte für die Wertetabelle unter dem Graphen.

Auch diese Tabelle wird über visiblePointsFrom aus einer unendlichen Werteliste herausgeschnitten.
Für die GUI werden standardmäßig die Werte von -5 bis 5 angezeigt.

-}

defaultTablePoints :: Poly -> [(Rational, Rational)]
defaultTablePoints poly = visiblePointsFrom defaultTablePointCount defaultTableStart defaultGraphStep poly

{- 

Diese Hilfsfunktion formatiert einen Punkt als String. Bsp: formatPoint (1, 2) = "1 | 2" 

Sie bleibt in Graph.hs und nicht in Format.hs, da sie spezifisch für die Darstellung von Punkten in der GUI 
ist und nicht allgemein für die Formatierung von Daten verwendet wird.

Durch Pretty a => und Pretty b => kann diese Funktion Punkte aus unterschiedlichen darstellbaren Typen formatieren.

-}

formatPoint :: (Pretty a, Pretty b) => (a, b) -> String
formatPoint (x, y) = pretty x ++ " | " ++ pretty y

{- 

Diese Hilfsfunktion f?llt einen String links mit Leerzeichen auf.

Das brauchen wir f?r die Wertetabelle, damit positive und negative Zahlen sauber untereinander stehen.
Wenn der String bereits lang genug ist, wird er unver?ndert zur?ckgegeben.

-}

padLeft :: Int -> String -> String
padLeft width textValue = replicate (width - length textValue) ' ' ++ textValue

{- 

Diese Funktion formatiert eine Liste von Punkten als Tabelle, die in der GUI angezeigt werden kann.
Sie bekommt als Eingabe eine Liste von Tupeln, die die Punkte darstellen, und gibt als Ausgabe einen String zur?ck, der die Tabelle darstellt.

Sie formatiert zuerst alle x- und y-Werte als Strings.
Danach berechnet sie die n?tige Spaltenbreite und f?llt k?rzere Werte mit Leerzeichen auf.
Dadurch verrutschen die Werte in der GUI nicht, auch wenn negative und positive Zahlen gemischt sind.

Sie bleibt in Graph.hs und nicht in Format.hs, da sie spezifisch f?r die Darstellung von Punkten in der GUI 
ist und nicht allgemein f?r die Formatierung von Daten verwendet wird.

-}

formatTable :: (Pretty a, Pretty b) => [(a, b)] -> String
formatTable points =
   let formattedPoints = [(pretty x, pretty y) | (x, y) <- points]
       xWidth = maximum (length "x" : map (length . fst) formattedPoints)
       yWidth = maximum (length "y" : map (length . snd) formattedPoints)
       header = padLeft xWidth "x" ++ " | " ++ padLeft yWidth "y"
       separator = replicate (xWidth + 3 + yWidth) '-'
       formatRow (xText, yText) = padLeft xWidth xText ++ " | " ++ padLeft yWidth yText
   in header ++ "\n" ++ separator ++ "\n" ++ unlines (map formatRow formattedPoints)

{-

Diese Funktion erzeugt einen kurzen Erklärungstext zur Lazy Evaluation.

Der Text wird später zusammen mit der Wertetabelle angezeigt.
Dadurch ist in der GUI sichtbar, dass der Graph nicht aus einer komplett vorberechneten Liste kommt,
sondern aus einer unendlichen Punktliste, aus der nur der benötigte Ausschnitt ausgewertet wird.

-}

lazyEvaluationInfo :: Int -> Rational -> Rational -> String
lazyEvaluationInfo pointCount start step =
   "Lazy Evaluation:\n"
   ++ "Die x-Werte werden als unendliche Liste mit iterate erzeugt.\n"
   ++ "Mit map wird daraus lazy eine unendliche Punktliste.\n"
   ++ "take " ++ show pointCount ++ " fordert nur die Punkte an, die wirklich angezeigt werden.\n"
   ++ "Startwert: " ++ pretty start ++ ", Schrittweite: " ++ pretty step

{- Hauptfunktion, die den Graphen in der GUi anzeigt -}

graphSvg :: Poly -> String
graphSvg poly = pointsToSvg (defaultPoints poly)

{- 

Diese Hilfsfunktin formatiert eine Liste von Punkten als SVG-Elemente, die in der GUI angezeigt werden können.
die Funktion bekommt als Eingabe eine Liste von Tupeln, die die Punkte darstellen, und gibt als Ausgabe einen String zurück, der die SVG-Elemente darstellt.

SVG-Elemente sind eine Art von Vektorgrafiken, die in HTML-Dokumenten verwendet werden können, um Grafiken darzustellen.

width und height sind die Breite und Höhe des SVG-Elements, margin ist der Abstand zwischen dem Rand des SVG-Elements und den Punkten, die dargestellt werden sollen.
points ist die Liste der Punkte, die dargestellt werden sollen.

Mithilfe von scalePoint werden die Punkte skaliert, um sie in den SVG-Elementen darzustellen.

Bsp: pointsToSvg [(1, 2), (3, 4), (5, 6)] gibt einen String zurück, der die SVG-Elemente darstellt, die die Punkte (1, 2), (3, 4) und (5, 6) darstellen.

-}

pointsToSvg :: [(Rational, Rational)] -> String
pointsToSvg points =
   let width = 520
       height = 260
       margin = 24
       coords = map (scalePoint width height margin points) points
       pointText = unwords [show x ++ "," ++ show y | (x,y) <- coords]
   in "<svg width='" ++ show width ++ "' height='" ++ show height ++ "' viewBox='0 0 "
      ++ show width ++ " " ++ show height ++ "' xmlns='http://www.w3.org/2000/svg'>"
      ++ "<rect width='100%' height='100%' fill='white' stroke='#cfd8e3'/>"
      ++ "<line x1='" ++ show margin ++ "' y1='" ++ show (height `div` 2)
      ++ "' x2='" ++ show (width - margin) ++ "' y2='" ++ show (height `div` 2)
      ++ "' stroke='#94a3b8'/>"
      ++ "<line x1='" ++ show (width `div` 2) ++ "' y1='" ++ show margin
      ++ "' x2='" ++ show (width `div` 2) ++ "' y2='" ++ show (height - margin)
      ++ "' stroke='#94a3b8'/>"
      ++ "<polyline fill='none' stroke='#006070' stroke-width='2' points='" ++ pointText ++ "'/>"
      ++ "</svg>"

{- 

Diese Funktion skaliert einen einzelnen Punkt (x, y) auf die Breite und Höhe des SVG-Elements, um ihn korrekt darzustellen.

Sie nimmt als Eingabe 3 Integer-Werte width, height und margin, die die Breite, Höhe und den Rand des SVG-Elements darstellen,
und eine Liste von Punkten points, die die Punkte darstellen, die im SVG-Element dargestellt werden

-}

scalePoint :: Int -> Int -> Int -> [(Rational, Rational)] -> (Rational, Rational) -> (Int, Int)
scalePoint width height margin points (x, y) =
   let xs = map fst points
       ys = map snd points
       minX = minimum xs
       maxX = maximum xs
       minY = minimum ys
       maxY = maximum ys
       plotWidth = fromIntegral (width - 2 * margin)
       plotHeight = fromIntegral (height - 2 * margin)
       scaleX = if maxX == minX then 0.5 else (x - minX) / (maxX - minX)
       scaleY = if maxY == minY then 0.5 else (y - minY) / (maxY - minY)
       px = fromIntegral margin + scaleX * plotWidth
       py = fromIntegral (height - margin) - scaleY * plotHeight
   in (round px, round py)

{- Diese Funktion erstellt eine kombinierte Darstellung des Graphen, der Lazy-Evaluation-Erklärung und einer Tabelle der Stützpunkte. -}

graphView :: Poly -> String
graphView poly =
   graphSvg poly
   ++ "<pre>"
   ++ lazyEvaluationInfo defaultTablePointCount defaultTableStart defaultGraphStep
   ++ "\n\n"
   ++ formatTable (defaultTablePoints poly)
   ++ "</pre>"
