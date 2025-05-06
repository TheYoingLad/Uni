module Orai13 where
import Data.Char
import Data.List
import Data.Maybe

which :: ([Char], [Char], [Char]) -> Char -> Int
which (a, b, c) x
    | elem x a = 1
    | elem x b = 2
    | elem x c = 3
    | otherwise = 0

matches :: (Int, Int) -> (Int, Int) -> Bool
matches (_ ,a) (b, _) = a == b

toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase (x:xs) = (toUpper x):xs

swap :: Maybe a -> b -> Maybe b
swap Nothing _ = Nothing
swap (Just a) b = Just b

numeric :: String -> Int
numeric [] = 0
numeric (x:xs)
    | x == 'r' = 4 + numeric xs
    | x == 'w' = 2 + numeric xs
    | x == 'x' = 1 + numeric xs

pythagoreans :: [(Int, Int, Int)]
pythagoreans = [(b,a,c) | a <- [1..100], b <- [1..a], c <- [a..(a+b)], a^2 + b^2 == c^2]

hasLongWord :: Int -> String -> Bool
hasLongWord n xs = longer 0 xs where
    longer m [] = m >= n
    longer m (x:xs)
        | m >= n = True
        | x /= ' ' = longer (m+1) xs
        | otherwise = longer 0 xs

align :: Int -> String -> String
align n xs
    | longer 0 xs = xs
    | otherwise = replicate (n - length xs) ' ' ++ xs where
        longer m [] = m >= n
        longer m (x:xs)
            | m >= n = True
            | otherwise = longer (m+1) xs

modify :: (a -> Maybe a) -> [a] -> [a]
modify _ [] = []
modify f (x:xs) = if not $ isJust (f x) then xs else (fromJust $ f x) : xs

isLonger :: [a] -> Int -> Bool
isLonger xs n = longer 0 xs where 
        longer m [] = m > n
        longer m (x:xs)
            | m > n = True
            | otherwise = longer (m+1) xs

removeAccents :: String -> String
removeAccents = map (\x -> case x of {'á' -> 'a'; 'é' -> 'e'; 'í' -> 'i'; 'ú' -> 'u'; 'ü' -> 'u'; 'ű' -> 'u'; 'ó' -> 'o'; 'ö' -> 'o'; 'ő' -> 'o'; _ -> x})

strip :: String -> String
strip = reverse . dropWhile (=='_') . reverse . dropWhile (=='_')

data RPS = Rock | Paper | Scissors
    deriving (Eq)

beats :: RPS -> RPS
beats Rock = Scissors
beats Scissors = Paper
beats Paper = Rock

firstBeats :: [RPS] -> [RPS] -> Int
firstBeats [] [] = 0
firstBeats (x:xs) (y:ys)
    | y == beats x = 1 + firstBeats xs ys
    | otherwise = firstBeats xs ys

data Temperature = Daytime Int | Night Int

isDaytime :: Temperature -> Bool
isDaytime (Daytime _) = True
isDaytime _ = False

extremes :: [Temperature] -> (Int, Int)
extremes xs = (maximum[n | (Daytime n) <- xs], minimum[n | (Night n) <- xs])