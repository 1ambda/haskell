module Homework10 where

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map(x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map(y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- exercise 0

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

-- exercise 1
removeOne :: Eq a => a -> [a] -> [a]
removeOne x [] = []
removeOne x (y:ys) | x == y = ys
                   | otherwise = y : removeOne x ys

-- exercise 2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeOne x ys)

-- exercise 3
split :: [a] -> [([a], [a])]
split [] = []
split [x] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]
