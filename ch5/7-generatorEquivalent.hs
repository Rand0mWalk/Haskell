concats xss = [x | xs <- xss, x <- xs]

res = concats [[(x,y) | y <- [3,4]] | x <- [1,2]]
--[[(x,y) | y <- [3,4]] | x <- [1,2]]
