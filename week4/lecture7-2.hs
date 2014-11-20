module Lecture7 where

import Data.Char
import Control.Monad

infixr 5 +++

-- ref: http://www.cs.nott.ac.uk/~gmh/Parsing.lhs
newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P $ \inp -> [(v, inp)]
  p >>= f = P $ \inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out

instance MonadPlus Parser where
  mzero = P $ \_ -> []
  p `mplus` q = P $ \inp -> case parse p inp of
                             [] -> parse q inp
                             [(v, out)] -> [(v, out)]

item :: Parser Char
item = P $ \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

ignore2 :: Parser (Char, Char)
ignore2 = do x <- item
             item
             z <- item
             return (x, z)

-- MonadPlus
failure :: Parser Char 
failure = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q


-- derived primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper 

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

int = (do char '-'
          n <- nat
          return (-n))
        +++ nat

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             return ()

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

nlist :: Parser [Int]
nlist = do symbol "["
           n <- natural
           ns <- many (do symbol ","
                          natural)
           symbol "]"
           return (n:ns)

-- arithmetic expressions
-- ref : http://www.cs.nott.ac.uk/~gmh/parser.lhs
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (t * f)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
           [(n, [])] -> n
           [(_, out)] -> error ("ununsed input: " ++ out)
           [] -> error ("invalid input: " ++ xs)
