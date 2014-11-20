-- ref: http://www.cs.nott.ac.uk/~gmh/monads

import Prelude hiding (Maybe, Just, Nothing, (>>=))

data Expr = Val Int | Div Expr Expr
data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just (n `div` m)

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
           Nothing -> Nothing
           Just x -> f x

eval :: Expr -> Maybe Int
eval (Val x) = Just x
eval (Div x y) = eval x >>= \n -> eval y >>= \m -> safediv n m

