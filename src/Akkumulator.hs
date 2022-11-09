module Akkumulator() where 

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

        
          

