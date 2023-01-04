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

-- 4) why are lazy-evaluation processes not used in imperative languagues?

-- lazy-evaluation is used to improve performance and to enable working with large datasets. 
-- This is possible due the fact that lazy-evaluation delays the calculation of a process until 
-- it is actualy needed. While variables or states can't be changed after execution in functional
-- languages this can happen in declarative languages. So the order in which statements are executed 
-- is important. That's why lazy-evaluation should not be used with declarative languages. 

-- 5) given following data-declaration: 

data Person = Student Name [(Fach, Note)]
type Fach = String
type Note = Double
-- (type Name is defined at the top of the file)

-- a) define a class for the following functions: 
-- durchschnitt -> calculates the average of all Noten from a Person.
-- besteNote    -> returns the best Note of a Person as a tuple of ([Fach], Note)

class Schule a where
    durchschnitt :: a -> Note
    besteNote :: a -> ([Fach], Note)

-- b) create an instance of the defined class and implement the function 
-- besteNote for every Person.

instance Schule Person where 
    besteNote person = besteNote' person ([], 6.0)
        where 
            besteNote' _ [] noten = noten
            besteNote' _ ((fach, note) : restliste) (faecher, best) 
                | note < best = besteNote' restliste ([], note)    
                | note == best = besteNote' restliste ((fach : faecher), best)
                | otherwise = besteNote' restliste (faecher, best) 

-- there are some difficulties with this function 
-- (does not work right atm) but that's okay for now.

-- c) define a test-Person and write a test-function for besteNote

-- Person "Jim" [("Math", 2.0), ("Physics", 1.9), ("Biology", 3.2)]
-- test :: Person -> ([Fach], Note)
-- test person = besteNote person

-- 6) find all mistakes in the following programm:

-- 1. data Zahl = Eins | Zwei | Drei 
-- 2. plus Eins Eins = Zwei
-- 3. plus Zwei Eins = Drei
-- 4. f :: [Zahl] -> Zahl -> Zahl
-- 5. f xs y = if xs == [] then y else (x + y)
-- 6.   where x :: Zahl
-- 7.         x = head xs

-- mistake line 1: the type Zahl is a completely new datatype which is not known by the class hierarchy of Haskell.
-- to display or compare these types we need to add other datatypes to the defined type. 
-- data Zahl = Eins | Zwei | Drei deriving (Show, Eq)

-- mistake line 5: x and y are defined as Zahl. It is not possible to use + on that datatype. 
-- Instead we could use the plus function as the following: 
-- f xs y = if xs == [] then y else (plus x y)

-- Lamda-expression for testing: (\Zahl Zahl -> plus Zahl Zahl) Eins Eins