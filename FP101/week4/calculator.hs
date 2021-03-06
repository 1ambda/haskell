-- ref: http://www.cs.nott.ac.uk/~gmh/

module Caculator where

import System.IO
import Data.Char
import Control.Monad

newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
  return v = P $ \inp -> [(v, inp)]
  p >>= f = P $ \inp -> case parse p inp of
                     [] -> []
                     [(v, out)] -> parse (f v) out

instance MonadPlus Parser where
  mzero = P $ \inp -> []
  p `mplus` q = P $ \inp -> case parse p inp of
                             [] -> parse q inp
                             [(v, out)] -> [(v, out)]


-- basic parsers

failure :: Parser a
failure = mzero

item :: Parser Char
item = P $ \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat  :: (Char -> Bool) -> Parser Char
sat p =  do x <- item
            if p x then return x else failure

digit :: Parser Char
digit =  sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum =  sat isAlphaNum

char :: Char -> Parser Char
char x =  sat (== x)

string :: String -> Parser String
string [] =  return []
string (x:xs) =  do char x
                    string xs
                    return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p =  do v  <- p
              vs <- many p
              return (v:vs)

ident :: Parser String
ident =  do x  <- lower
            xs <- many alphanum
            return (x:xs)

nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
        +++ nat

space :: Parser ()
space =  do many (sat isSpace)
            return ()

token  :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

identifier :: Parser String
identifier =  token ident

natural :: Parser Int
natural =  token nat

integer :: Parser Int
integer =  token int

symbol :: String -> Parser String
symbol xs =  token (string xs)

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ do symbol "-"
                  e <- expr
                  return (t - e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           +++ do symbol "/"
                  t <- term
                  return (f `div` t)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ natural

-- derived primitives

getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC["  ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO () 
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

-- calculator
box :: [String]
box =  ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]
                 
buttons :: [Char]
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = 
  seqn [writeAt (1, y) line | (y, line) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeAt (3, 2) "             "
                writeAt (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons
               then process c xs
               else do beep
                       calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
           [(n, "")] -> calc (show n)
           _ -> do beep
                   calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
