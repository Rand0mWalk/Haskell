safetaila :: [a] -> [a]
safetaila xs = if null xs then [] else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs  = []
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc [] = []
safetailc xs = tail xs
