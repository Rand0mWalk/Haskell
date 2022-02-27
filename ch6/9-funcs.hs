sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

lst :: [a] -> a
lst (x:xs) | null xs = x
           | otherwise = lst xs

--Using idiomatic pattern matching
lsta :: [a] -> a
lsta [x] = x
lsta (x:xs) = lsta xs
