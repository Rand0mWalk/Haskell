euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m > n = euclid (m-n) n
           | otherwise = euclid m (n-m)
