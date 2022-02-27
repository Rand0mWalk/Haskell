merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take h xs, drop h xs)
                where h = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:xs) | null xs = [x]
             | otherwise = merge (msort fh) (msort sh)
                            where fh = fst (halve (x:xs))
                                  sh = snd (halve (x:xs))
