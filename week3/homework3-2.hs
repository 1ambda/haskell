sum1 = sum' 0
      where
        sum' v [] = v
        sum' v (x:xs) = sum' (v + x) xs

reverse' = foldl (\xs x -> x:xs) []

-- composition of a list of functions
-- compose [(+1), (+1), (+1)] 3 == 6
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
