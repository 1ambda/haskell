sum1 = sum' 0
      where
        sum' v [] = v
        sum' v (x:xs) = sum' (v + x) xs

reverse' = foldl (\xs x -> x:xs) []

-- composition of a list of functions
-- compose [(+1), (+1), (+1)] 3 == 6
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- exercise 1

all' :: (a -> Bool) -> [a] -> Bool
-- all' p xs = and (map p xs)
-- all' p = and . map p
-- all' p = not . any (not . p)
all' p = foldl (&&) True . map p

-- exercise 2

any' :: (a -> Bool) -> [a] -> Bool
-- any' p = or . map p
-- any' p xs =  length (filter p xs) > 0
-- any' p = not . null . dropWhile (not . p)
-- any' p xs = not (all (\x -> not (p x)) xs)
-- any' p xs = foldr(\x acc -> (p x) || acc) False xs
any' p xs = foldr (||) True (map p xs)

-- exercise 3
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- exercise 4
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
 | p x = dropWhile p xs
 | otherwise = x:xs

-- exercise 5
map' :: (a -> b) -> [a] -> [b]
map' f = foldl(\xs x -> xs ++ [f x]) []

-- exercise 6
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) [] 

-- exercise 7
dec2int :: [Integer] -> Integer
dec2int = foldl(\acc x -> acc * 10 + x) 0

-- exercise 9
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

-- exercise 10
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- exercise 11
unfold p h t x
 | p x = []
 | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

-- exercise 12
map2 f = unfold null (f . head) tail

-- exercise 13
iterate' f = unfold (const False) id f -- const False is pred. always return False

