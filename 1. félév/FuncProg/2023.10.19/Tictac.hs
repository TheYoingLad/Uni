module Tictac where
import Data.Char
import Data.List

type Cell = (Int, Int)
type Grid = [Cell]
type Player = Char
type State = ([Cell], [Cell], Int)

cells :: Int -> Grid
cells 0 = []
cells x
    | x < 0 = []
    | otherwise = sort[(n,m) | n<-[0..(x-1)], m<-[0..(x-1)]]

rows :: Int -> [[Cell]]
rows 0 = []
rows x
    | x < 0 = []
    | otherwise = sort[[(m,n) | n<-[0..(x-1)]]| m<-[0..(x-1)]]

cols :: Int -> [[Cell]]
cols 0 = []
cols x
    | x < 0 = []
    | otherwise = sort[[(n,m) | n<-[0..(x-1)]]| m<-[0..(x-1)]]

diags :: Int -> [[Cell]]
diags 0 = [[],[]]
diags x = sort[[(n,m) | n<-[0..(x-1)], m<-[0..(x-1)], n == m], 
               [(n,m) | n<-[0..(x-1)], m<-[0..(x-1)], (n + m) == (x-1)]]

validatePlayer :: (Player -> a) -> Player -> a
validatePlayer x y
    | elem y ['o', 'O', 'x', 'X'] = x y
    | otherwise = error (concat ["illegal player ", (show y)])

otherPlayer :: Player -> Player
otherPlayer x
    | elem x ['o', 'O'] = 'X'
    | elem x ['x', 'X'] = 'O'
    | otherwise = error (concat ["illegal player ", (show x)])

initState :: Int -> State
initState x = ([], [], x)

isValidCell :: Int -> Cell -> Bool
isValidCell x (a,b) = a <= (x-1) && b <= (x-1)

isCellFree :: State -> Cell -> Bool
isCellFree (x, y, _) a = not ((elem a x) || (elem a y))

isValidStep :: State -> Cell -> Bool
isValidStep a@(_, _, x) b = (isCellFree a b) && (isValidCell x b)

step :: Player -> State -> Cell -> State
step x y@(a, b, c) z
    | not (isValidStep y z) = error (concat ["invalid move ", (show z), " of player ", (show x)])
    | (validatePlayer toUpper x) == 'O' = ((z:a), b, c)
    | otherwise = (a, (z:b), c)

freeCells :: State -> [Cell]
freeCells (a, b, c) = [n | n <- [(l,m) | l <- [0..(c-1)], m <- [0..(c-1)]], not ((elem n a) || (elem n b))]

steps :: Player -> State -> [State]
steps x y = [n | m <- (freeCells y), n <- [step (validatePlayer toUpper x) y m]]

combN :: Ord a => Int -> [a] -> [[a]]
combN x = filter ((x ==) . length) . subsequences . sort

won :: Player -> State -> Bool
won x y@(a,b,c) 
    | elem x ['O', 'o'] = or[elem n (rows c) || elem n (cols c) || elem n (diags c) | n <- (combN c a)]
    | otherwise = or[elem n (rows c) || elem n (cols c) || elem n (diags c) | n <- (combN c b)]

fork :: (Player, [State], [State]) -> (Player, [State], [State])
fork (x, y, z) = ((otherPlayer x), b, z ++ a) where
    a = [n | n <- y, won x n || won (otherPlayer x) n]
    b = concatMap (steps x) [n | n <- y, not ((won x n) || (won (otherPlayer x) n))]

get2 :: (a,b,c) -> b
get2 (_,x,_) = x

play :: Player -> [State] -> [State]
play _ [] = []
play x y = (play (otherPlayer x) (get2(fork (x, a, b)))) ++ b where
    a = [n | n <- y, not ((won x n) || (won (otherPlayer x) n))]
    b = [n | n <- y, won x n || won (otherPlayer x) n]