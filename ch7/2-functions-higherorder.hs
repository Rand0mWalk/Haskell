all' :: (a -> Bool) -> [a] -> Bool
all' f xs = (and . map f) xs 

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = and . map f

all''' :: (a -> Bool) -> [a] -> Bool
all''' f [] = True
all''' f (x:xs) = f x && all''' f xs

all'''' :: (a -> Bool) -> [a] -> Bool
all'''' f = foldr (\x y -> f x && y) True


any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = or (map f xs)

any'' :: (a -> Bool) -> [a] -> Bool
any'' _ [] = False
any'' f (x:xs) = f x || any'' f xs

any''' :: (a -> Bool) -> [a] -> Bool
any''' f = foldr (\x y -> f x || y) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                   | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
--takeWhile'' f = foldr (\x xs -> if f x then x:xs else []) []
takeWhile'' f = foldr (\x acc -> if f x then x:acc else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                   | otherwise = x : xs
--define in terms of fold
--dropWhile'' :: (a -> Bool) -> [a] -> [a]
