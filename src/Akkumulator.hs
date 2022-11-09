module Akkumulator() where 

-- Durch die Akkumulator-Technik kann, durch speichern des Zwischenergebnis im Akkumulator, ein Iterativer Prozess abgebildet werden. 

-- Berechnet die Fakultät
faculty :: Integer -> Integer
faculty n = faculty' n 0
    where 
        faculty' 0 ergebnis = ergebnis
        faculty' n ergebnis = faculty' (n - 1) (n^2 + ergebnis)


-- Berechnet die Fakultät
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' n 0 1
    where 
    fibonacci' 0 akk1 akk2 = akk1
    fibonacci' 1 akk1 akk2 = akk2
    fibonacci' n akk1 akk2 = fibonacci' (n - 1) akk2 (akk1 + akk2) 
    

-- Nimmt eine Liste von Noten [(Matrikelnummer, Note)] entgegen & berechnet den Durchschnitt aller Noten.
durchschnitt :: [(Integer, Double)] -> Double
durchschnitt notenliste = durchschnitt' notenliste 0 0
        where 
            durchschnitt' [] summe anzahl = summe / anzahl
            durchschnitt' ((_, note) : restliste) summe anzahl = durchschnitt' restliste (summe + note) (anzahl + 1)


-- Nimmt eine Liste von Noten [(Matrikelnummer, Note)] entgegen & sammelt alle Matrikelnummern als Liste, welche die Note 1.0 haben. 
bestnoten :: [(Integer, Double)] -> [Integer]
bestnoten liste = bestnoten' liste []
        where 
            bestnoten' [] mnummern = mnummern
            bestnoten' ((mnummer, 1.0) : restliste) mnummern = bestnoten' restliste (mnummer : mnummern)
            bestnoten' ((_, _) : restliste) mnummern = bestnoten' restliste mnummern