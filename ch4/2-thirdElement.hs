thirda :: [a] -> a
thirda xs = head (tail (tail xs)) 

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc (_:_:x:_) = x
