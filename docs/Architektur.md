# Architektur

Diese Datei beschreibt grob, wie das Projekt aufgebaut ist.

Die Idee war, die Berechnung, Darstellung und GUI nicht komplett in eine Datei zu schreiben. Deshalb ist das Projekt in mehrere Module aufgeteilt.

## Grundidee

Die Polynomlogik liegt im Kern in Poly.hs. Dort werden Monome und Polynome definiert und die wichtigsten Operationen umgesetzt.

Die GUI soll diese Funktionen nur benutzen. Sie soll möglichst wenig eigene Mathematik enthalten. Dadurch bleibt die Rechenlogik besser testbar.

## Wichtige Module

Poly.hs enthält die Datentypen Monom und Poly. Dort liegen auch Normalisieren, Addieren, Subtrahieren, Multiplizieren, Ableiten, Auswerten und Division.

ParserSimple.hs ist für die Eingabe zuständig. Die Eingabe kommt als Text aus der GUI und wird dort in ein Polynom umgewandelt. Fehler werden mit Either zurückgegeben.

Format.hs enthält Funktionen, die Werte lesbar machen. Dazu gehören mathematische Schreibweise, LaTeX und die allgemeine Baumformatierung.

Display.hs baut längere Ausgabetexte zusammen. GUI.hs muss dadurch nicht jede Darstellung selbst zusammensetzen.

Tree.hs wandelt Polynome in Ausdrucksbäume um. Diese Bäume werden später für Baumdarstellung, Analyse und Traversierung benutzt.

Analysis.hs wertet Informationen über einen Baum aus, z.B. Anzahl von Knoten oder Tiefe.

Animation.hs erzeugt Traversierungsschritte. Diese Schritte können in der GUI nacheinander angezeigt werden.

Graph.hs erzeugt Punkte für den Graphen und eine Wertetabelle. Dabei wird mit einer unendlichen Liste gearbeitet, aus der nur ein Ausschnitt genommen wird.

Library.hs speichert Polynome mit Namen. Das ist die Polynomliste, die der Benutzer in der GUI sieht.

History.hs speichert vergangene Berechnungen. Die History ist sichtbar und kann über einen Button angezeigt werden.

Cache.hs speichert bereits berechnete Operationen intern. Wenn dieselbe Operation nochmal ausgeführt wird, kann das Ergebnis wiederverwendet werden.

Parallel.hs enthält die normale und parallele Auswertung mehrerer Polynome.

Random.hs erzeugt zufällige Polynome.

Template.hs und Examples.hs enthalten feste Beispielpolynome, die mit Template Haskell erzeugt werden.

GUI.hs verbindet alles mit der Threepenny-Oberfläche. Dort werden Buttons, Eingabefelder, Statusleiste, Ergebnisbereich und Polynomliste erstellt.

AppMain.hs startet die Anwendung.

## Ablauf in der GUI

Der Benutzer gibt ein Polynom als Text ein.

ParserSimple.hs wandelt diesen Text in ein Poly um.

Das Polynom wird in Library.hs mit einem Namen gespeichert.

Wenn der Benutzer eine Operation auswählt, holt GUI.hs die ausgewählten Polynome aus der Library.

Die eigentliche Berechnung passiert dann meistens in Poly.hs oder Parallel.hs.

Das Ergebnis wird als GuiResult gespeichert, damit die Darstellungsbuttons später darauf zugreifen können.

Display.hs, Format.hs, Tree.hs, Analysis.hs oder Graph.hs erzeugen danach die gewünschte Ausgabe.

## Zustand

Die GUI benutzt IORef für Zustände, die sich während der Benutzung ändern.

Dazu gehören:

- die gespeicherten Polynome
- die ausgewählten Polynome
- das letzte Ergebnis
- die History
- der Cache

Reine Berechnungen liegen trotzdem möglichst außerhalb der GUI. Dadurch können sie einfacher getestet werden.

## Fehlerbehandlung

Viele Funktionen geben Fehler nicht durch einen Programmabbruch zurück, sondern über Either, Maybe oder über GuiActionResult.

In der GUI wird bei einem Fehler die Statusleiste rot angezeigt. Außerdem wird das alte Ergebnis bei Rechenfehlern zurückgesetzt, damit keine alte Darstellung weiterbenutzt wird.

## Tests

Die Tests liegen im Ordner test.

Es gibt eigene Testmodule für Poly, Parser, Library, Cache, Graph, History, Parallel und Tree.

Der Einstiegspunkt ist PolyTest.hs. Von dort werden die anderen Testmodule eingebunden.
