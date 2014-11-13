zero = \s z -> z
one = \s z -> s z
two = \s -> s . s
five = \s -> s . s . s . s . s
six = \s -> (s . s) . (s . s) . (s . s)

c2i x = x (+1) 0
c2s x = x ('*' :) ""

add x y = \s z -> x s (y s z)
mul x y = \s z -> x (y s) z
