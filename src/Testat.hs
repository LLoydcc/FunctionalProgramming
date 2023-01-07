module Testat where

-- 1) why is the following code incorrect?
-- f x y = (==) [(1.3,'a')] x ++ y

-- (==) [(1.3, 'a')] x is checking for equality between the tuple and value x
-- with ++ the result of the equality check is concatenated to the value of y
-- this won't work because ++ is looking for strings to concatenate but the first
-- expression is a boolean value.

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


-- 5) indicate the datatypes of the following functions:
--    (>) :: Ord a => a -> a -> Bool  
-- a) (>)
--    (++[1..10]) :: (Num a, Enum a) => [a] -> [a]
-- b) (++[1..10])
--    f :: Num a => a -> a
-- c) f x = (\x -> x + 1) x

-- 6) find the mistakes in the following code: 
-- f x y = (== 3) x y
-- g (x:xs) = (\x -> (++) [x] xs)

-- mistake 1 (f): in function f (== 3) is applied to 2 arguments x y 
-- but it can only be applied to one argument. 
-- mistake 2 (g): in the lambda function it is being attempted to concatenate [x] 
-- with xs. This implies that x is a list but x, as the head of the given list, is not a list
-- but a value. 