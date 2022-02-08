['a','b','c'] == ['a','b','c'] :: [Char]
('a','b','c') == ('a','b','c') :: [Char,Char,Char]
[(False,'O'),(True,'1')] == ([(False,'O'),(True,'1')] :: [(Bool,Char)])
([False,True],['0','1']) == ([False,True],['0','1']) :: ([Bool],[Char])
[tail, init, reverse] ==  [tail, init, reverse] :: [[a]->[a]] 
