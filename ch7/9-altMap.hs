altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f f' [] = []
altMap f f' [x] = [f x]
altMap f f' (x1:x2:xs) = f x1 : f' x2 : altMap f f' xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ [] = []
altMap' f f' (x:xs) = f x : altMap' f' f xs
