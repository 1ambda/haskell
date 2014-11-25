module Lecture9 where

import Prelude hiding (Left, Right, Up, Down)

-- type decl
type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

type Trans = Pos -> Pos

left :: Trans
left (x, y) = (x-1, y)

type Pair a = (a, a)

mult :: Pair Int -> Int
mult (a, b) = a * b

copy :: Int -> Pair Int
copy a = (a, a)

-- multiple parameters
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k xs = head [v | (k', v) <- xs, k == k']

-- doesn't work
-- we can only define a recursive type using `data` keyword
-- type Tree = (Int, [Tree])

-- data decl
-- data Bool = False | True

data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Move = Left | Right | Up | Down

move :: Move -> Pos -> Pos
move Up (x, y) = (x, y-1)
move Left (x, y) = (x-1, y)
move Down (x, y) = (x, y+1)
move Right (x, y) = (x+1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

data Shape = Circle Float
           | Rect Float Float

square :: Float -> Shape 
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

-- recursive type

-- list
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons h t) = 1 + len t

-- natural numbers
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) s = Succ (add n s)


-- arithmetic expression
data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add l r) = size l + size r
size (Mul l r) = size l + size r

eval :: Expr -> Int
eval (Val n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

data Tree = Leaf Int
          | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs n (Leaf k) = n == k
occurs n (Node l k r) =
  (n == k) 
  || occurs n l
  || occurs n r

flatten :: Tree -> [Int]
flatten (Leaf k) = [k]
flatten (Node l k r) = flatten l ++ [k] ++ flatten r

-- occurs for search-ree
occurs' :: Int -> Tree -> Bool
occurs' n (Leaf k) = k == n
occurs' n (Node l k r) | n == k = True
                       | n < k = occurs' n l
                       | otherwise = occurs' n r


-- class

-- class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y = not (x == y)

-- instance Eq Bool where
--   False == False = True
--   True == True = True
--   _ == _ = False

-- class Eq a => Ord a where
--   (<), (<=), (>), (>=) :: a -> a -> Bool
--   min, max :: a -> a -> a

--   min x y | x <= y = x
--           | otherwise = y

--   max x y | x <= y = y
--           | otherwise = x

-- class Ord Bool where
--   False < True = True
--   _ < _ = False

--   b > c = c < b
--   b <= c = (b < c) || (b == c)
--   b >= c = c <= b

-- data Bool = False | True
--           deriving (Eq, Ord, Show, Read)

  
