import Data.Char

-- exercise 0
sumOfSquares :: Int -> Int
sumOfSquares n = sum [x^2 | x <- [1..n]]

-- exercise 1
replicateC :: Int -> a -> [a]
replicateC n v = [v | _ <- [1..n]]

-- exercise 2
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- exercise 3
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(init(factors x)) == x]

-- exercise 5
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

-- exercise 6
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- xs `zip` ys] 

-- exercise 7
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a' + 26
          | otherwise = ord c - ord 'A'

int2let :: Int -> Char
int2let n | n < 26 = chr (ord 'A' + n)
          | otherwise = chr (ord 'a' + n - 26)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let(((let2int c + n) `mod` 26) + 26)
          | isUpper c = int2let(((let2int c + n) `mod` 26)) 
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
                                 
-- exercise 12
riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]
