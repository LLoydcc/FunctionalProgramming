module Funktionsaufrufe where
summe x y = if y <= x then y else y + summe x (y - 1)
anzahl x y = y - x + 1
mittelwert x y = fromIntegral(summe x y) / fromIntegral(anzahl x y)
quersumme :: Int -> Int
quersumme x | x < 10 = x
            | otherwise = x `mod` 10 + quersumme (x `div` 10)
bonbons :: Int -> Int -> Int 
bonbons geld bonbonanzahl | (bonbonanzahl * 10) > geld || (bonbonanzahl * 10) >= 100 = bonbonanzahl
                          | otherwise = bonbons (geld - (bonbonanzahl * 10)) (bonbonanzahl + 1)