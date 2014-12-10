-- from
-- http://www.reddit.com/r/programming/comments/225f0/beautiful_haskell_implementation_of_maths_power
subset [] = return []
subset (x:xs) = do xs' <- subset xs
                   [x:xs', xs']

powerset [] = [[]]
powerset (x:xs) = xs' ++ map (x:) xs'
  where xs' = powerset xs
