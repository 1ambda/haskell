f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
           where
	     ys = [a | a <- xs, a <= x]
	     zs = [b | b <- xs, b > x]

avg ns = div (sum ns) (length ns)

zeroto :: Int -> [Int]
zeroto n = [0..n]

add :: Int -> (Int -> Int)
add x y = x + y
add3 = add 3

second xs = head (tail xs)
swap (a, b) = (b, a)
pair a b = (a, b)

double x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)

g xs = take 3 (reverse xs)

abs2 n = if n >= 0 then n else -n
signum2 n = if n > 0 then 1 else
	      if n == 0 then 0 else -1

abs3 n | n >= 0 = n
       | otherwise = -n

not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool

True && True = True
_ && _ = False

head1 (x : _) = x
tail1 (_ : xs) = xs

add1 = \x -> x + 1

addxy = \x -> (\y -> x + y)

const x _ = x

odds n = map f [0..n]
          where
	    f x = x `mod` 2 /= 0

odds1 n = map (\x -> x `mod` 2 /= 0) [0..n]

isDigit c = c >= '0' Prelude.&& c <= '9'

fst = \(a, b) -> a
snd (_, y) = y