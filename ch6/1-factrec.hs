--If defined without guard the function doesn't terminate for negative values. However, if defined with guard then it complains of non-exhaustive pattern for negative value
fact :: Int -> Int
fact n | n > 0 = n * fact (n-1)
       | n == 0 = 1
