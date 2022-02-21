import Data.Char

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x' == x ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x==x']

let2int :: Char -> Int
let2int c | isLower c  = ord c - ord 'a'
          | isUpper c  = ord c - ord 'A'
          | otherwise  = ord c

int2lets :: Int -> Char
int2lets n = chr (ord 'a' + n)

int2letC :: Int -> Char
int2letC n = chr (ord 'A' + n)

shift :: Int -> Char -> Char 
shift n c | isLower c = int2lets (mod (let2int c + n) 26)
          | isUpper c = int2letC (mod (let2int c + n) 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--Deciphering
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x lowerc) n | x <- ['a'..'z']]
            where n = length xs
                  lowerc = [toLower y | y <- xs]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o-e) ^ 2 / e| (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where factor = head (positions (minimum chitab) chitab)
          chitab = [chisqr (rotate n table') table | n <- [0..25]]
          table' = freqs xs
