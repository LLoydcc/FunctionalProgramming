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

-- Aufgabe 2.2)
-- Rekursive Funktion (Fibonacci) im Vergleich zur Iterativen Funktion
-- (:set +s gibt Zeitaufwand der Funktionen aus) 
-- fib_rek 25 = (0.27 secs, 64,854,128 bytes)
fib_rek :: Int -> Int
fib_rek n | n == 0 = 0
          | n == 1 = 1
          | otherwise = fib_rek(n - 1) + fib_rek(n - 2)

-- Iterative Funktion
-- fib_iter 25 = (0.00 secs, 447,624 bytes)
fib_iter :: Int -> Int
fib_iter n | n == 0 = 0
           | n == 1 = 1
           | otherwise = fib_it n 1 0
           where
               fib_it n x y = if (n == 1) then x
               else fib_it (n - 1) (x + y) x
