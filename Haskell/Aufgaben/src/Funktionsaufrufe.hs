module Funktionsaufrufe where
-- Aufgabe 1)
-- Rekursive Funktion zur Berechnung einer Summe - z.B.: x = 1, y = 3, dann Summe = 6 
-- (1 + 2 + 3 = 6)
summe x y = if y <= x then y else y + summe x (y - 1)

-- Bestimmung der Anzahl von Zahlen zwischen 2 Zahlen (x und y).
-- x = 1, y = 2, dann Anzahl = 3
anzahl x y = y - x + 1

-- Funktion zur Berechnung des Durchschnitts von x und y
mittelwert x y = fromIntegral(summe x y) / fromIntegral(anzahl x y)

-- Aufgabe 2)
-- Rekursive Funktion zur Berechnung der Quersumme x 
-- z.B.: 456 -> 4 + 5 + 6 = 15
quersumme :: Int -> Int
quersumme x | x < 10 = x
            | otherwise = x `mod` 10 + quersumme (x `div` 10)

-- Aufgabe 3)
-- Wir besitzen eine Menge x an Geld (Param 'geld') und können davon Bonbons kaufen. 
-- Der erste Bonbon kostet 10cent, der zweite 20cent, der dritte 30cent usw. bishin zu 1€. 
-- Wieviele Bonbons können wir uns ingesamt kaufen, bis der Rest-Geldbetrag nicht mehr ausreicht?
bonbons :: Int -> Int -> Int 
bonbons geld bonbonanzahl | (bonbonanzahl * 10) > geld || (bonbonanzahl * 10) >= 100 = bonbonanzahl 
                          | otherwise = bonbons (geld - (bonbonanzahl * 10)) (bonbonanzahl + 1)