module Homework11 where

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
fib n = fibs !! n

largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)

repeat' :: a -> [a]
repeat' x = xs
  where xs = x : xs

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
               where t = repeatTree x
