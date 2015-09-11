-- exercise 0

(^!) :: Int -> Int -> Int
m ^! 0 = 1
m ^! n = m * m ^! (n - 1)

-- exercise 4

ands :: [Bool] -> Bool
-- ands [] = True
-- ands (b:bs) = b && ands bs

-- ands [] = True
-- ands (b:bs) | b = ands bs
--             | otherwise = False

-- ands [] = True
-- ands (b: bs) | b == False = False
--              | otherwise = ands bs

ands [] = True
ands (b:bs) = ands bs && b

-- exercise 5

concatC :: [[a]] -> [a]
concatC [] = []
concatC (xs:xss) = xs ++ concat xss

-- exercise 7

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)


-- exercise 9
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y then x : merge xs (y: ys) else y : merge (x:xs) ys

-- exercise 10
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where (ys, zs) = halve xs



