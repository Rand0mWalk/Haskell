halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
              where mid = length xs `div` 2

halve' :: [a] -> ([a],[a])
halve' xs = splitAt (length xs `div` 2) xs
