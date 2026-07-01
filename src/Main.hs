module Main where

import Poly
import qualified Control.Applicative as Strings

{-

Dies dient hier nicht zu echten automatischen Tests, sondern nur als kleine Konsolendemo.

Wir schreiben eine main-Funktion, die Etwas ausführt, sobald man "main" im ghci eintippt.
In diesem Fall ist es sinnvoll, um bestimmte Polynomfunktionen aus Poly.hs auszuführen.

putStrLn ist wie System.out.println("Hallo"), es geht nur für Strings.
print ist wie System.out.println("Hallo"), es geht für alles, was eine Show-Instanz hat, also z.B deriving (Show, ...)

-}

main :: IO()
main = do

    let p1 = 3 #^ 2 + 2 #^ 1 + 1 -- Darstellung mit #, nutzt Num-Instanz: p1 = 3x^2 + 2x + 1, interne auch so Darstellbar: P [M 3 2, M 2 1, M 1 0]
    let p2 = 1 #^ 1 + 1 -- Darstellung mit #, nutzt ebenfalls Num-Instanz: p2 = 1x + 1, intern auch so Darstellbar: P [M 1 1, M 1 0]

    putStrLn "Functional Polynomial Interpreter:"
    putStrLn "---------------------------------"

    putStrLn "Erstes Polynom:"
    putStr "Intern: "
    print p1
    putStrLn ("LaTeX: " ++ toLaTeX p1)

    putStrLn "---------------------------------"

    putStrLn "Zweites Polynom:"
    putStr "Intern: "
    print p2
    putStrLn ("LaTeX: " ++ toLaTeX p2)

    putStrLn "---------------------------------"

    putStrLn "Normalisierung des ersten Polynoms:"
    putStr "Intern: "
    print (normalize p1)
    putStrLn ("LaTeX: " ++ toLaTeX (normalize p1))

    putStrLn "---------------------------------"

    putStrLn "Normalisierung des zweitem Polynoms:"
    putStr "Intern: "
    print (normalize p2)
    putStrLn ("LaTeX: " ++ toLaTeX (normalize p2))

    putStrLn "---------------------------------"

    putStrLn "Addition der Polynome:"
    putStr "Intern: "
    print (p1 + p2)
    putStrLn ("LaTeX: " ++ toLaTeX (p1 + p2))

    putStrLn "---------------------------------"

    putStrLn "Subtraktion der Polynome:"
    putStr "Intern: "
    print (p1 - p2)
    putStrLn ("LaTeX: " ++ toLaTeX (p1 - p2))

    putStrLn "---------------------------------"

    putStrLn "Negierung des ersten Polynoms:"
    putStr "Intern: "
    print (negate p1)
    putStrLn ("LaTeX: " ++ toLaTeX (negate p1))

    putStrLn "---------------------------------"

    putStrLn "Negierung des zweiten Polynoms:"
    putStr "Intern: "
    print (negate p2)
    putStrLn ("LaTeX: " ++ toLaTeX (negate p2))

    putStrLn "---------------------------------"

    putStrLn "Multiplikation der Polynome:"
    putStr "Intern: "
    print (p1 * p2)
    putStrLn ("LaTeX: " ++ toLaTeX (p1 * p2))

    putStrLn "---------------------------------"

    putStrLn "Ableitung des ersten Polynoms:"
    putStr "Intern: "
    print (derivation p1)
    putStrLn ("LaTeX: " ++ toLaTeX (derivation p1))

    putStrLn "---------------------------------"

    putStrLn "Ableitung des zweiten Polynoms:"
    putStr "Intern: "
    print (derivation p2)
    putStrLn ("LaTeX: " ++ toLaTeX (derivation p2))

    putStrLn "---------------------------------"

    putStrLn "Auswertung des ersten Polynoms an der Stelle x = 2:"
    putStr "Intern: "
    print ((§) p1 2)
    putStrLn ("LaTeX: " ++ toLaTeX ((§) p1 2))

    putStrLn "---------------------------------"

    putStrLn "Auswertung des zweiten Polynoms an der Stelle x = 4:"
    putStr "Intern: "
    print ((§) p2 4)
    putStrLn ("LaTeX: " ++ toLaTeX ((§) p2 4))

    putStrLn "---------------------------------"

    putStrLn "Polynomdivision des ersten Polynoms durch das zweite Polynom:"
    putStr "Intern: "
    print (p1 /% p2)

    let (q, r) = p1 /% p2

    putStrLn ("Quotient: " ++ toLaTeX q)
    putStrLn ("Rest: " ++ toLaTeX r)