dec2int :: [Int] -> Int
-- Perfect way to understand fold and accumulator for complex functions
dec2int = foldl (\acc x -> acc*10 + x) 0
