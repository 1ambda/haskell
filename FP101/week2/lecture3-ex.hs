k x = x * 3
z (x, y) = x
h x y = x * y 

-- problem 8

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1)  xs ++ drop x xs

-- problem 7

remove1 n xs = take n xs ++ drop (n + 1) xs -- not

-- import Prelude hiding ((&&))

-- problem4

-- True && True = True
-- _ && _ = False

-- a && b = if a then if b then True else False else False

-- not
-- a && b = if not (a) then not (b) else True

-- no else
-- a && b = if a then b

-- not
-- a && b = if a then if b then False else True else False

-- a && b = if a then b else False

-- a && b = if b then a else False

-- map cmpand [(False, False), (True, False), (False, True), (True, True)]
cmpand (a, b) = a && b


-- problem3
-- import Prelude hiding ((||))

-- use this function to test || implementation
-- map cmpor [(False, False), (True, False), (False, True), (True, True)]
cmpor (a, b) = a || b

-- False || False = False
-- True || True = True
-- False || True = True
-- True || False = True

-- b || True = b -- not
-- _ || True = True

-- b || c
--   | b == c = c
--   | otherwise = True

-- b || False = b
-- _ || True = True

-- b || c
--   | b == c = b
--   | otherwise = True

-- False || b = b
-- True || _ = True

-- False || False = False
-- _ || _ = True

-- b || c -- not
--   | b == c =  True
--   | otherwise = False


-- problem 2

safeTail1 xs = if null xs then [] else tail xs

safeTail2 [] = []
safeTail2 (_ : xs) = xs

safeTail4 xs
  | null xs = []
  | otherwise = tail xs


safeTail6 [] = []
safeTail6 xs = tail xs

safeTail8 = \ xs ->
	      case xs of
	        [] -> []
		(_ : xs) -> xs

-- safeTail3 (_ : xs) -- not
--   | null xs = []
--  | otherwise = tail xs


-- safeTail5 xs = tail xs -- not
-- safeTail5 [] = []

-- safeTail7 [x] = [x] -- not
-- safeTail7 (_ : xs) = xs


-- problem 1

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
            where n = length xs

halve6 xs = splitAt (div (length xs) 2) xs


halve8 xs = (take n xs, drop n xs)
       	    where n = length xs `div` 2

	   
-- halve1 xs = (take n xs, drop n xs)
--            where n = length xs / 2

-- halve7 xs = splitAt (length xs / 2) xs

-- halve4 xs = spliatAt (length xs `div` 2)

-- halve5 xs = (take n xs, drop (n + 1) xs)
--       	    where n = length xs `div` 2