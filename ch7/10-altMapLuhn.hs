altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ [] = []
altMap' f f' (x:xs) = f x : altMap' f' f xs

luhn :: [Int] -> Bool
luhn nums = ((sum $ altMap' (\x -> if 2*x > 9 then (2*x)-9 else 2*x) (*1) (init nums) ) + last nums)  `mod` 10 == 0

-- Just to input numbers in space format - could be done better
cardnums a b c d e f g h i j k l m n o p = luhn [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
