import Data.Char

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs =
  and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = (length xs) - 1
                       
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

lengthC :: [a] -> Int
lengthC xs = sum[1 | _ <- xs]

find :: Eq a => a -> [(a, b)] -> [b]
find k ts = [v | (k', v) <- ts, k == k']

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c == c']
