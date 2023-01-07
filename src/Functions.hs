module Functions where

    -- write a function that checks for palindroms ("otto" or "anna" is a palindrom)
    palindrom :: String -> Bool
    palindrom word = word == rev word
        
    rev :: String -> String
    rev word = rev' word ""
        where
            rev' "" result = result
            rev' (head : restliste) result = rev' restliste (head : result)


    -- we have a value of money and we can buy items with it. 
    -- The first item costs 10cents, the second 20cents, 30cents etc. up until 1€.
    -- We can buy every item just one time, after we bought it, the next item costs more.
    -- Write a function that calculates how many items we can buy.
    moneybank :: Integer -> Integer
    moneybank money = moneybank' money 10 0
        where
            moneybank' money centv count 
                | money < centv || centv >= 100 = count           
                | otherwise = moneybank' (money - centv) (centv + 10) (count + 1) 