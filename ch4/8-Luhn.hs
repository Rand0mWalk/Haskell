luhnDouble :: Int -> Int
--luhnDouble x = if x*2>10 then (x*2)-9 else x*2
luhnDouble x | x*2>10 = (x*2)-9
             | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (sum (map luhnDouble [a,c])+b+d) `mod` 10 == 0
--Wrong implementation of a more general version - revisit someday
--luhn x = ((x `mod` 10)+luhnDouble (x `mod` 100)+(x `mod` 1000)+ luhnDouble (x `mod` 10000)) `mod` 10 == 0


