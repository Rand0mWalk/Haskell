--revqsort::[a]->[a]
revqsort [] = []
revqsort (n:ns) = revqsort bigger ++ [n] ++ revqsort smaller
                  where 
                    bigger = [a | a <- ns, a>=n]
                    smaller = [b | b <-ns, b<n]
