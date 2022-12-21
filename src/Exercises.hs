module Exercises where

type Telefon = Integer
type Name    = String

-- 1) 
-- write a function to convert a list of tuples [(Name, Telefon)] to a tuple of lists ([Name], [Telefon]).

-- a) recursive 
f_rec :: [(Name, Telefon)] -> ([Name], [Telefon])
f_rec [] = ([], [])
f_rec ((name, telefon) : restliste) = 
  let (names, telefons) = f_rec restliste
  in (name : names, telefon : telefons)


-- b) iterativ
f_iter :: [(Name, Telefon)] -> ([Name], [Telefon])
f_iter liste = f_iter' liste ([], [])
  where 
    f_iter' [] result = result
    f_iter' ((name, telefon) : restliste) (names, telefons) = f_iter' restliste (name : names, telefon : telefons)

-- c) recursion 
--    advantages: clarity (easy to read mathematically + write formulas etc.).
--    disadvantages: higher memory consumption (each recursive call needs a stack entry). 

-- 2)
-- f = 
--   let g = umwandeln (\ x → x + x) in g (reverse [1..4])
--     umwandeln _ [] = []
--     umwandeln k (x:xs) = k x : umwandeln k xs
-- aufruf = foldr (++) f

-- a) what is the result of f?
-- the result of f is [8, 6, 4, 2].

-- explanation: we call 'umwandeln' with 2 parameters -> a function k (defined as \x -> x + x) and a list g [4, 3, 2, 1].
-- in every call k takes the head of the list x and calculates x + x. 
-- After that the tail of the list will be called recusively in 'umwandeln'.

-- b) define a polymorph type for 'f', 'aufruf' and 'g'.

-- f :: Num a => [a]
-- f = 
--   g :: Num a => a -> a
--   let g = umwandeln (\ x -> x + x) in g (reverse [1..4])
--  umwandeln _ [] = []
--  umwandeln k (x:xs) = k x : umwandeln k xs

-- aufruf :: Num a => [[a]] -> [a]
-- aufruf = foldr (++) f

-- 3) describe the calculation process for function f and the value f (1+1) 1 with the help of the substitution model.
-- f :: Integer -> Integer -> Integer
-- f x 0 = x
-- f x y = f x 0 + f (y - 1) x

-- > f (1 + 1) 1
--   f 2 1
--   f (2 0) + (f (1 - 1) 2)
--   2 + (f 0 2)
--   2 + ((f 0 0) + (f 1 0))
--   2 + 0 + 1
--   = 3