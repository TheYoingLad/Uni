module Orai11 where
import Data.List

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ _ acc [] = acc
foldr_ f acc (x:xs) = f x (foldr_ f acc xs)

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ _ acc [] = acc
foldl_ f acc (x:xs) = foldl_ f (f acc x) xs

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

concat' :: [[a]] -> [a]
concat' = foldl (++) []

length' :: [a] -> Int
length' = foldl (\acc x -> acc + 1) 0

sumsq :: Integer -> Integer
sumsq n = foldr (\x acc -> x^2 + acc) 0 [1..n]

elem' :: Eq a => a -> [a] -> Bool
elem' n = foldr (\x acc -> if acc then True else x == n) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

isort :: Ord a => [a] -> [a]
isort = foldr (\x acc -> insert x acc) []

plusplus :: [a] -> [a] -> [a]
plusplus = flip $ foldr (\x acc -> x:acc)

count :: (a -> Bool) -> [a] -> Int
count p = foldr (\x acc -> if p x then acc + 1 else acc) 0

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' p = foldr (\x acc -> if p x then acc else False) True

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = foldr (\y acc -> if acc then x == y else False) True xs

maxlist :: Ord a => [a] -> a
maxlist (x:xs) = foldr (\y acc -> if y > acc then y else acc) x xs

fromBin :: [Int] -> Integer
fromBin = fst . foldr (\x (acc,exp) -> (acc + (fromIntegral x) * 2 ^ exp, exp + 1)) (0,0)

head' :: [a] -> a
head' = foldr (\x acc -> x) (error "empty list")

last' :: [a] -> a
last' = foldl (\acc x -> x) (error "empty list")