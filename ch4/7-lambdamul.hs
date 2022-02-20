mult :: Int -> Int -> Int
mult = \x -> (\y -> (\z -> z*y*x))
