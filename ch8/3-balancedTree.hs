data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a -> Int
leaves (Leaf x) = 1
leaves (Node lt rt) = leaves lt + leaves rt

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node lt rt) = abs (leaves lt - leaves rt) <=1 && balanced lt && balanced rt
