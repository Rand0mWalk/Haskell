{-
length [1,2,3]
1 + length [2,3]
1 + 1 + lenght [3]
1 + 1 + 1 + length []
1 + 1 + 1 + 0 = 3

drop 3 [1,2,3,4,5]
= {applying drop}
drop 2 [2,3,4,5]
= {applying drop}
drop 1 [3,4,5]
= {applying drop}
drop 0 [4,5]

init [1,2,3]
= {applying init}
1: init [2,3]
= {applying init}
1:2: init [3]
= {applying init}
1:2:[]
= [1,2]
-}