module Orai9 where

--lambda fgv

f :: Int -> Int -> Int
f x y = x + y

-- (->) jobbra köt

g :: Int -> (Int-> Int)
g x = h where
    h :: Int -> Int
    h y = x + y

g' :: Int -> (Int-> Int)
g' x = \y -> x + y

applyToOne'' :: (Int -> Int) -> Int
applyToOne'' f = f 1

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:(filter' f xs)
    | otherwise = filter' f xs

(#) :: (b -> c) -> (a -> b) -> a -> c
(#) f g x = f $ g x

-- éta radukció: 
alma :: Int -> Int
--alma x = div 10 x
alma = div 10
--éta ekvivalencia: \x -> f x == f

doStuff :: [Int] -> [Int]
doStuff = map (div 10) . filter (> 2) . map (+ 1)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:(takeWhile' f xs)
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile' (== x) xs) : group (dropWhile' (== x) xs)

groupBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f (x:xs) = (x : takeWhile' f xs) : groupBy f (dropWhile' f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []

mapWithIndex' :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex' f = zipWith' f [0..]

filterWithIndex' :: (Int -> a -> Bool) -> [a] -> [a]
filterWithIndex' f = map snd . filter (\(x,y) -> f x y) . zip [0..]
--filterWithIndex' f xs = [x | (i, x) <- (zip [0..] xs), f i x]

--splitOn :: Eq a => a -> [a] -> [[a]] ----------HF-----------------

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):xs) = catMaybes xs
catMaybes ((Just x):xs) = x:catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs)
    | f x == Nothing = mapMaybe f xs
    | f x == Just y = y : (mapMaybe f xs)