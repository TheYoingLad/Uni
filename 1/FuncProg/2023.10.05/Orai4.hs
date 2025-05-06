module Orai4 where

oraPerc :: [(Int, Int)]
oraPerc = [(x, y) | x <- [0..23], y <- [0..59]]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = null[n | n <- [2..x-1], mod x n == 0]

countSpace :: [Char] -> Int
countSpace x = sum[ 1 | ' ' <- x]

pizza :: [(String, Int)] -> Int
pizza x = sum(snd (unzip x)) + 500 

null' :: [a] -> Bool
null' (a:b) = False
null' _ = True

head' :: [a] -> a
head' (a:b) = a

tail' :: [a] -> [a]
tail' (a:b) = b

isSingleton :: [a] -> Bool
isSingleton [a] = True
isSingleton _ = False

headZero :: (Num a, Eq a) => [a] -> Bool
headZero a = head a == 0

fact 0 = 1
fact n = n * fact(n-1)

sumN 0 = 0
sumN n = n + sumN(n-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

length' :: [a] -> Int
length' [] = 0
length' x = 1 + length' (tail x)

last' :: [a] -> a
last' [a] = a
last' (a:b) = last' b

init' :: [a] -> [a]
init' [a] = []
init' (a:b) = a:(init' b)