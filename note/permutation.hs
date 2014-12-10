
-- interleave 0 [1, 2]
-- [0, 1, 2] [1, 0, 2] [1, 2, 0]

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- permutate [1, 2, 3]
-- [1, 2, 3] [1, 3, 2], ...
permutate :: [a] -> [[a]]
permutate [] = [[]]
permutate (x:xs) = concat (map (interleave x) (permutate xs))
