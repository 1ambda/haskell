-- ref : http://www.haskellforall.com/2012/12/the-continuation-monad.html

import Control.Monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r}

-- i.e Cont (IO ()) String
onInput :: (String -> IO ()) -> IO ()
onInput f = forever $ do
  str <- getLine
  f str


-- pythagoras : http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

square :: Int -> Int
square x = x * x

add :: Int -> Int -> Int
add x y = x + y


-- usage: add_cps 3 4 print
-- :t print
-- Show a => a -> IO (), so r == IO ()
add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \cont -> cont (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \cont -> cont (square x)

square_cps2 :: Int -> ((Int -> r) -> r)
square_cps2 x cont = cont (square x)

foo :: Int -> Int
foo x y = x + y

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
  square_cps x $ \xSq ->
  square_cps y $ \ySq ->
  add_cps xSq ySq k

-- usage: seven show
-- seven :: (Int -> r) -> r
seven = \f -> f 7

-- usage: seven incr incr show
-- incr :: Int -> (Int -> r) -> r
incr = \y f -> f $ 1 + y

