sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

lst :: [a] -> a
lst (x:xs) | null xs = x
           | otherwise = lst xs
