module Graph where

import Poly
import Format (Pretty(..))

{- Dieses Modul dient dazu, einen Graphen zu visualisieren, sie enthält die ganze Logik. -}


{-

Diese Funktion berechnet für eine Liste von x-Werten die entsprechenden y-Werte für 
ein gegebenes Polynom und gibt eine Liste von Tupeln zurück, die die Punkte darstellen.

Sie bekommt als Eingabe ein Polynom und eine Liste von x-Werten und gibt als Ausgabe eine Liste von 
Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

-}

samplePoints :: Poly -> [Rational] -> [(Rational, Rational)]
samplePoints poly xs = map (\x -> (x, evaluate poly x)) xs


{- 

Diese Funktion berechnet für ein gegebenes Polynom eine unendliche Liste von Punkten, die auf dem Graphen des Polynoms liegen.
Sie bekommt als Eingabe ein Polynom und gibt als Ausgabe eine unendliche Liste von Tupeln zurück, die die Punkte darstellen, die auf dem Graphen des Polynoms liegen.

x ist hierbei eine unendliche Liste von Rationalen Zahlen, die von -10 bis unendlich geht mithilfe von Lazy Evaluation.

Sinn dieser Funktion ist es, die Punkte des Graphen des Polynoms zu berechnen, um sie in der GUI anzuzeigen.

-}

infinitePoints :: Poly -> [(Rational, Rational)]
infinitePoints poly = [(x, evaluate poly x) | x <- [(-10)..]]

{- 

Diese Funktion zeigt die ersten n Punkte des Graphen eines Polynoms an, die auf dem Graphen des Polynoms liegen.
Sie nimmt mithilfe von take n Punkte aus der unendlichen Liste von Punkten, die von der Funktion infinitePoints zurückgegeben wird.

-}

visiblePoints :: Int -> Poly -> [(Rational, Rational)]
visiblePoints n poly = take n (infinitePoints poly)


{- 

Diese Funktion erzeugt einen Standardausschnitt von Punkten für ein gegebenes Polynom, um den Graphen in der GUI anzuzeigen.
Standard ist hier dann [-10 .. 10] für x-Werte und die entsprechenden y-Werte werden berechnet.

-}

defaultPoints :: Poly -> [(Rational, Rational)]
defaultPoints poly = visiblePoints 21 poly 

{- 

Diese Hilfsfunktion formatiert einen Punkt als String. Bsp: formatPoint (1, 2) = "1 | 2" 

Sie bleibt in Graph.hs und nicht in Format.hs, da sie spezifisch für die Darstellung von Punkten in der GUI 
ist und nicht allgemein für die Formatierung von Daten verwendet wird.

-}

formatPoint :: (Pretty a, Pretty b) => (a, b) -> String
formatPoint (x, y) = pretty x ++ " | " ++ pretty y

{- 

Diese Funktion formatiert eine Liste von Punkten als Tabelle, die in der GUI angezeigt werden kann.
Sie bekommt als Eingabe eine Liste von Tupeln, die die Punkte darstellen, und gibt als Ausgabe einen String zurück, der die Tabelle darstellt.

Sie benutzt die Hilfsfunktion formatPoint, um jeden Punkt in der Liste zu formatieren und fügt dann die Kopfzeile "x | y" und eine Trennlinie hinzu.

Sie bleibt in Graph.hs und nicht in Format.hs, da sie spezifisch für die Darstellung von Punkten in der GUI 
ist und nicht allgemein für die Formatierung von Daten verwendet wird.

-}

formatTable :: (Pretty a, Pretty b) => [(a, b)] -> String
formatTable points = "x | y\n" ++ "-----\n" ++ unlines (map formatPoint points)

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


{- Diese Funktion erstellt eine kombinierte Darstellung des Graphen und einer Tabelle der Stützpunkte. -}
graphView :: Poly -> String
graphView poly = graphSvg poly ++ "<pre>" ++ formatTable (samplePoints poly [-5..5]) ++ "</pre>"