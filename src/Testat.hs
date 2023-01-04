module Testat where

-- 1) why is the following code incorrect?
-- f x y = (==) [(1.3,'a')] x ++ y

-- 2) define a type for the following local function f:
-- f :: Num a => a -> [a] -> [a]
g = let f = \x y -> 1 : x : y in f 3 [2.5, 4]

--3) write a function that turns a list of integers within a iterative 
-- process into the opposite direction: [1, 6, 3, 8] -> [8, 3, 6, 1] 

reverse :: [Integer] -> [Integer]
reverse list = reverse' list []
        where
            reverse' [] result = result
            reverse' (head : restliste) result = reverse' restliste (head : result)

-- 4) define a function that takes a tuple (Integer, [(String, Double)]) and calculates the average
-- of the Double values.

average :: (Integer, [(String, Double)]) -> Double
average list = average' list 0 0
        where 
          average' (_, []) sum count = sum / count
          average' (mn, ((_, note) : restliste)) sum count = average' (mn, restliste) (sum + note) (count + 1)