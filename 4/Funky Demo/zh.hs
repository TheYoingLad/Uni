module ZH where
import Data.Char

-- A
--1
splitQuadruple :: (a,b,c,d) -> ((a,b),(c,d))
splitQuadruple (a,b,c,d) = ((a,b),(c,d))

--2
inInterval :: Integer -> Integer -> [Integer] -> [Integer]
inInterval _ _ [] = []
inInterval a f (x:xs)
    | f < a = []
    | x >= a && x <= f = x : (inInterval a f xs)
    | otherwise = inInterval a f xs

--3
lengthOfShorter :: [a] -> [b] -> Integer
lengthOfShorter as bs = helper (zip as bs) 0 where
    helper [] n = n
    helper (x:xs) n = helper xs (n+1)

--4
compressLetters :: String -> String
compressLetters [] = []
compressLetters (x:y:xs)
    | x == y && isLower(x) && isLower(y) = toUpper(x) : compressLetters(xs)
    | otherwise = x : compressLetters(y:xs)
compressLetters xs = xs

------------------------------------

--B
--1
tripleToLists :: (a,a,a) -> [[a]]
tripleToLists (a,b,c) = [[a],[b],[c]]


filterCloseTo :: Integer -> Integer -> [Integer] -> [Integer]
filterCloseTo o r ls
    | r < 0 = []
    | otherwise = [n | n <- ls, n >= o-r, n <= o+r]

shorterStringLength :: Num a => String -> String -> a
shorterStringLength s1 s2 = helper (zip s1 s2) 0 where
    helper [] n = n
    helper (x:xs) n = helper xs (n+1)

addAdjacentEquals :: [Integer] -> [Integer]
addAdjacentEquals [] = []
addAdjacentEquals (x:y:xs)
    | x == y = (x+y) : addAdjacentEquals(xs)
    | otherwise = x : addAdjacentEquals(y:xs)
addAdjacentEquals xs = xs
