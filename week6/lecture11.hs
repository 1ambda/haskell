module Lecture11 where

ones :: [Int]
ones = 1 : ones

primes :: [Int]
primes = seive [2..]

seive :: [Int] -> [Int]
seive (p : xs) = p : [x | x <- xs, x `mod` p /= 0]

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

sumWith :: Int -> [Int] -> Int
sumWith v [] = v
sumWith v (x:xs) = (sumWith $! (v + x)) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = (foldl' f $! (f v x)) xs

sumWith' :: Int -> [Int] -> Int
sumWith' = foldl' (+)
