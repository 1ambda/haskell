factorial 0 = 1
factorial n = n * factorial(n - 1)

productC :: [Int] -> Int
productC [] = 1
productC (n : ns) = n * productC ns

lengthC :: [a] -> Int
lengthC [] = 0
lengthC (x : xs) = 1 + length xs

reverseC :: [a] -> [a]
reverseC [] = []
reverseC (x : xs) = reverse(xs) ++ [x]

zipC :: [a] -> [b] -> [(a, b)]
zipC [] _ = []
zipC _ [] = []
zipC (x:xs) (y:ys) = (x, y) : zip xs ys

dropC :: Int -> [a] -> [a]
dropC 0 xs = xs
dropC _ [] = []
dropC n (x:xs) = drop (n-1) xs

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

replicateC :: Int -> a -> [a]
replicateC 0 x = []
replicateC n x = x : (replicateC (n-1) x)

-- multiply
(***) :: Int -> Int -> Int
m *** 0 = 0
m *** (n) = m + (m *** (n-1))

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

evenC :: Int -> Bool
evenC 0 = True
evenC n = oddC (n-1)

oddC :: Int -> Bool
oddC 0 = False
oddC n = evenC (n-1)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs
