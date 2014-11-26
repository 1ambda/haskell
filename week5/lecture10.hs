module Lecture10 where

data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y 
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1

data Expr = Val Int | App Op Expr Expr

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- subs [1, 2] -> [[], [1], [2], [1, 2]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- interleave 1 [2, 3] -> [[1, 2, 3], [2, 1, 3], [2, 3, 1]]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- perm [1, 2, 3] = [[1, 2, 3], [1, 3, 2], [2, 3, 1], ..]
perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = concat (map (interleave x) (perm xs))

-- choices [1, 2] -> [[], [1], [2], [1, 2], [2, 1]]
choices :: [a] -> [[a]]
choices xs = concat (map (perm) (subs xs))

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- brute force
split :: [a] -> [([a], [a])]
split xs = [splitAt i xs | i <- [1..(n-1)]]
  where n = length xs

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- brute force solutions
bSolutions :: [Int] -> Int -> [Expr]
bSolutions ns n = [e | ns' <- choices ns
                     , e <- exprs ns'
                     , eval e == [n]]

-- fast version
type Result = (Expr, Int)

-- [(e, n) | e <- exprs ns
--         , n <- eval e]
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns
                  , lx <- results ls
                  , ry <- results rs
                  , res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) =
  [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div]
                            , valid o x y]

fastSolutions :: [Int] -> Int -> [Expr]
fastSolutions ns n = [e | ns' <- choices ns
                       , (e, m) <- results ns'
                       , m == n]
