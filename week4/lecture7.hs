module Lecture7 where


import Prelude hiding (return)

type Parser a = String -> [(a, String)]

item :: Parser Char
item = \xs -> case xs of
               [] -> []
               (y:ys) -> [(y, ys)]

return :: a -> Parser a
return v = \xs -> [(v, xs)]

failure :: Parser a
failure = \xs -> []

parse :: Parser a -> String -> [(a, String)]
parse p xs = p xs

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \xs -> case p xs of
                  [] -> []
                  [(y, ys)] -> [(y, ys)]

-- then
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= q = \xs -> case parse p xs of
                  [] -> []
                  [(y, ys)] -> parse (q y) ys

p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x, y)

-- sat :: (Char -> Bool) -> Parser Char
-- sat p =  do x <- item
--             if p x then Lecture7.return x else failure
