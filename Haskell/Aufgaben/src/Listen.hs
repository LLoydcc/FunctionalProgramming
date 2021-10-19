module Listen where
-- Aufgabe 1)
-- Rekursive Funktion unter Anwendung der Haskell Funktionen 'take' und 'drop'
-- zum ausgeben einer Unter-Liste einer bestehenden Liste 
-- Params: Startindex, Endindex, Liste
unterListe :: Int -> Int -> [Int] -> [Int]
unterListe n m a = take m (drop n a)

-- Aufgabe 2) 
-- Rekursive Funktion zur Bestimmung der Fibonacci Folge mit anschließender Ausgabe
-- aller Werte innerhalb eines Arrays
fibListe :: Int -> [Int]
fibListe x | x == 0 = [0]
           | x == 1 = [1, 0]
           | otherwise = (head(fibListe (x - 1)) + head(fibListe(x - 2))) : fibListe(x - 1) 
