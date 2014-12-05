module Lab5 where

import Control.Monad

{- 
A process is represented by the following recursive algebraic data type Action
that encodes primitive actions that perform a side-effect and then
return a continuation action, or the concurrent execution of two actions,
or an action that has terminated.

To suspend a process, we need to grab its "future" and store it away for later use.
Continuations are an excellent way of implementing this. We can change a function into
continuation passing style by adding an extra parameter, the continuation,
that represents the "future" work that needs to be done after this function terminates.

Instead of producing its result directly,
the function will now apply the continuation to the result. 
-}
data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

data Concurrent a = Concurrent ((a -> Action) -> Action)

{-
This type can be read as a function that takes as input a continuation function (a -> Action),
that specifies how to continue once the result of type a of the current computation is available.
An application f c of this type will call c with its result when it becomes available. 
-}

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

{-Ex. 0
To express the connection between an expression of type Concurrent a and one of type Action,
we define a function

action :: Concurrent a -> Action that transforms a ((a -> Action) -> Action)

into an Action that uses Stop :: Action to create the continuation to the Concurrent
a passed as the first argument to action.
-}

-- action' :: ((a -> Action) -> Action) -> Action
-- action' = \a -> Stop

action :: Concurrent a -> Action
action cont = \a -> cont a


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = error "You have to implement stop"


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork = error "You have to implement fork"

par :: Concurrent a -> Concurrent a -> Concurrent a
par = error "You have to implement par"


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

