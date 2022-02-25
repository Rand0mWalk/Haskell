and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs 

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a :  replicate' (n-1) a

pickel :: [a] -> Int -> a
pickel (x:xs) 0 = x
pickel (x:xs) n = pickel xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x = True
              | otherwise = elem' a xs
