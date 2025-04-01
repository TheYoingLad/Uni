module Orai1 where

one :: Int
one = 1

inc :: Int -> Int
inc x = x + one

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

isEven :: Int -> Bool
isEven x = mod x 2 == 0

min' :: Int -> Int -> Int
min' x y
    | x < y = x
    | x > y = y
    | otherwise = x

isOdd :: Int -> Bool
isOdd x = not (isEven x)

odavissza :: Int[] -> Int[]
odavissza x[]