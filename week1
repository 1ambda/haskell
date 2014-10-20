lastC xs = head (reverse xs)
double x = x * x

sumC [] = 0
sumC (x : xs) = x + sum(xs)

productC [] = 1
productC (x: xs) = x * productC(xs)

qsort [] = []
qsort (x : xs) = (qsort small) ++ [x] ++ (qsort large)
                 where small = [a | a <- xs, a <= x]
                       large = [b | b <- xs, b > x]
                   
qsortRev [] = []
qsortRev (x : xs) = reverse (reverse (qsortRev small)++ [x] ++ reverse (qsortRev large))
                    where small = [a | a <- xs, a <= x]
                          large = [b | b <- xs, b > x]

factoral n = product [1 .. n]
average ns = sum ns `div` length ns

{-
  nested comment
-}
