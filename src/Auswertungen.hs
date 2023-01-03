module Auswertungen() where

-- Welchen Typ hat die folgende Funktion g?

-- g :: [Integer] -> Integer -> Integer -> Integer
-- g (x : xs) y z = x + y 
-- g xs y z = if xs == z then y else head xs + y

mult :: Integer -> Integer -> Integer
mult x = \y -> x * y