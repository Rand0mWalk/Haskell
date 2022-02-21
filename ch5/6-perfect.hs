factors :: Int -> [Int]
factors x = [fac | fac<-[1..x-1], x `mod` fac == 0]

perfects :: Int -> [Int]
perfects n = [p | p<-[1..n], sum (factors p) == p ]
