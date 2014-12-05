-- ref : https://www.haskell.org/tutorial/io.html

main :: IO ()
main = do c <- getChar
          putChar c

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n'
    then return ""
    else do cs <- getLine'
            return (c:cs)

-- this list doesn't actually invoke nay actions. It simply holds them.
todoList :: [IO ()]
todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]

sequence' :: [IO ()] -> IO ()
sequence' = foldr (>>) (return ())

-- In Haskell, the map function does not perform nay action.
-- instead it creates a list of actions consumed by putStr'
putStr' :: String -> IO ()
putStr' s = sequence' (map putChar s)

-- exception handling
