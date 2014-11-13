-- higher-order function

twice :: (a -> a) -> a -> a
twice f x = f (f x)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' :: [a] -> Int
length' = foldr' (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldr' (\x xs -> xs ++ [x]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr' (\x acc -> if p x then x : acc else acc) [] xs

map' :: (a -> b) -> [a] -> [b]
map' p xs = foldr' (\x acc -> p x : acc) [] xs

odd' = not . even

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]

all2 :: (a -> Bool) -> [a] -> Bool
all2 p xs = foldr (\x acc -> p x && acc) True xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = [] 

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs
