module Lecture12 where

repl :: Int -> a -> [a]
repl 0 _ = []
repl n x = x : repl (n-1) x
-- repl (n + 1) x = x : repl n x

append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x : append' xs ys

-- reverse' xs ys = reverse xs ++ ys
reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

-- improved. linear time
ireverse :: [a] -> [a]
ireverse xs = reverse' xs []

-- tree
data Tree = Leaf Int | Node Tree Tree
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

flatten' t ns = flatten t ++ [ns]

-- compiler correctness

data Expr = Val Int | Add Expr Expr
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

instance Show Expr where
  show (Val n) = "(Val " ++ show n ++ ")"
  show (Add x y) = "(Add " ++ show x ++ " " ++ show y ++ ")"

data Op = PUSH Int | ADD
type Stack = [Int]
type Code = [Op]

instance Show Op where
  show (PUSH n) = "PUSH " ++ show n
  show (ADD) = "ADD"

exec :: Code -> Stack -> Stack
exec []         s       = s
exec (PUSH n:c) s       = exec c (n:s)
exec (ADD:c)    (m:n:s) = exec c (m+n:s)

-- compile' :: Expr -> Code
-- compile' (Val n) = [PUSH n]
-- compile' (Add x y) = compile x ++ compile y ++ [ADD]

-- compile :: Expr -> Code
-- compile e = compile' e

-- compile' e c = compile e ++ c
compile' :: Expr -> Code -> Code
compile' (Val n)   c = PUSH n : c
compile' (Add x y) c = compile' x (compile' y (ADD : c))

compile e = compile' e [] 

e :: Expr
e = (Add (Add (Val 2) (Val 3)) (Val 4))
