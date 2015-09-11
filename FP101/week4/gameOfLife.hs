-- ref: http://www.cs.nott.ac.uk/~gmh/life.lhs
module GameOfLife where

cls :: IO ()
cls =  putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs =  do goto p
                   putStr xs

seqn :: [IO a] -> IO ()
seqn [] =  return ()
seqn (a:as) =  do a
                  seqn as


-- game of life

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showCells :: Board -> IO ()
showCells b = seqn [writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) =  map wrap [(x-1,y-1), (x,y-1),
                           (x+1,y-1), (x-1,y),
                           (x+1,y)  , (x-1,y+1),
                           (x,y+1)  , (x+1,y+1)] 

wrap :: Pos -> Pos
wrap (x,y) =  (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))

liveNeighbs :: Board -> Pos -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbs b p) [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
            isEmpty b p,
            liveNeighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) xs

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showCells b
            wait 5000
            life (nextGen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
