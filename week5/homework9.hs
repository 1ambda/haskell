module Homework9 where

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

-- exer 0
nat2int :: Nat -> Integer
nat2int = \n -> genericLength [c | c <- show n, c == 'S']

-- exer 1
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- exer 2
add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ m) = Succ (add m n)

-- exer 3
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- exer 4

data Tree = Leaf Integer
          | Node Tree Integer Tree

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n = True
  | m < n = occurs m l
  | otherwise = occurs m r

-- exer 5

data Tree1 = Leaf1 Integer
          | Node1 Tree1 Tree1

leaves (Leaf1 _) = 1
leaves (Node1 l r) = leaves l + leaves r
balanced :: Tree1 -> Bool
balanced (Leaf1 _) = True
balanced (Node1 l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

unbalTree :: Tree1
unbalTree = Node1 (Node1 (Node1 (Leaf1 3) (Leaf1 5)) (Leaf1 2)) (Leaf1 3)

balTree :: Tree1
balTree = Node1 (Node1 (Leaf1 1) (Leaf1 2)) (Leaf1 3)

-- exer 6

balance :: [Integer] -> Tree1
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf1 x
balance xs = Node1 (balance ys) (balance zs)
  where (ys, zs) = halve xs



