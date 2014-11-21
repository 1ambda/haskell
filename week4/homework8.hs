-- ref: http://members.chello.nl/hjgtuyl/tourdemonad.html#filterM

module Homework8 where

import Control.Monad

-- exercise 1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

-- exercise 2

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

-- exercise 3

getLine' :: IO String
getLine' = get ""

get :: String -> IO String
get xs = do x <- getChar
            case x of
             '\n' -> return xs
             _ -> get (xs ++ [x])

-- exercise 4
interact' f = do input <- getLine'
                 putStrLn' (f input)

-- exercise 5
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return () 
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

-- or
-- sequence_' ms = foldr (>>) (return ()) ms

-- exercise 6
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do a <- m
                      as <- sequence' ms
                      return (a:as)

-- exercise 7
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

-- exercise 8
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do flag <- p x
                       ys <- filterM' p xs
                       if flag then return (x:ys) else return ys

-- exercise 11
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = do x <- m
                return (f x)

