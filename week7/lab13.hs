------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================


-- h: head, r: rest
root :: Rose a -> a
root (h :> r) = h

children :: Rose a -> [Rose a]
children (h :> r) = r 

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

tree0 = 'x' :> map (flip (:>) []) ['a'..'x']
ex0 = length $ children tree0
tree1 = 'x' :> map (\c -> c :> []) ['a'..'A']
ex1 = length (children tree1)
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (h :> r) = 1 + sum (map size r)

leaves :: Rose a -> Int
leaves (h :> []) = 1
leaves (h :> r)  = sum (map leaves r)

tree3 = 1 :> map (\c -> c :> []) [1..5]
ex3 = size tree3

ex4 = size . head . children $ tree3
ex5 = leaves tree3

ex6 = product (map leaves (children tree3))
ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  -- fmap :: (a -> b) -> m a -> m b
  fmap f (h :> r) = f h :> map (fmap f) r

ex8 = size (fmap leaves (fmap (:> []) tree3))
ex9 r = fmap head $ fmap (\x -> [x]) r
ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Monoid (Sum a) where
  mempty = Sum 0 
  mappend x y = Sum ((unSum x) + (unSum y))
  
instance Num a => Monoid (Product a) where
  mempty = Product 1 
  mappend x y = Product((unProduct x) * (unProduct y))

unSum :: Sum a -> a
unSum (Sum x) = x
unProduct :: Product a -> a
unProduct (Product x) = x

ex11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap g a = fold $ fmap g a

-- ref: https://github.com/asterkin/fp101-haskell/blob/master/rose-trees.hs
instance Foldable Rose where
  fold (h :> r) = (f.g) r `mappend` h
    where g = map (fold)
          f = foldr (mappend) mempty

tree14 = 1 :> [2 :> [], 3 :> [4 :> []]]
tree14' = fmap Product tree14

ex14 = unProduct $ fold tree14' 
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================
tree16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
ex16 = unSum $ foldMap Sum tree16

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum r = unSum $ foldMap Sum r
fproduct r = unProduct $ foldMap Product r

ex19 = fsum xs
ex20 = fproduct xs

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

