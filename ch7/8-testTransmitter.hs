-- Copied code from previous problem and extended to solution
import Data.Char --to use ord
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\acc x -> acc + 2*x) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addparity :: [Bit] -> [Bit]
addparity bits | (odd . length . filter (==1)) bits = bits ++ [1]
               | otherwise  = bits ++ [0]

encode :: String -> [Bit]
encode = concat . map (addparity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkparity :: [Bit] -> [Bit]
checkparity bits | (odd . length . filter (==1) . take 8) bits && (bits !! 8 == 1)  = take 8 bits
                 | (even . length . filter (==1) . take 8) bits && (bits !! 8 == 0)  = take 8 bits
                 | otherwise = error "Error in parity bits, aborting transmission"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkparity) . chop9  

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultychannel :: [Bit] -> [Bit]
faultychannel = tail

faultytransmit :: String -> String
faultytransmit = decode . faultychannel . encode
