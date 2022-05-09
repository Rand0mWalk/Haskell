--data Ordering = LT | EQ | GT

--compare :: Ord a => a -> a -> Ordering

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf x) = compare a x == EQ
--occurs a (Node l x r) = occurs a l || a == x || occurs a r
occurs a (Node l x r) | compare a x == EQ = True
                      | compare a x == LT = occurs a l
                      | compare a x == GT = occurs a r
