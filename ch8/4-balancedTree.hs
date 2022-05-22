data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Read)


split :: [a] -> ([a],[a])
split [] = error "Empty list provided"
split [x] = ([x],[])
split xs = splitAt (length xs `div` 2 ) xs

balance :: [a] -> Tree a
balance [x] = ( Leaf x )
balance xs = ( Node (balance (fst (split xs))) (balance (snd (split xs))) )
