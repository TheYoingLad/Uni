module Homework where

insertWithPredicate :: (a -> a -> Bool) -> a -> [a] -> [a]
insertWithPredicate p1 a xs = takeWhile (not . p1 a) xs ++ [a] ++ dropWhile (not . p1 a) xs

filterMulti :: [(a -> Bool)] -> [a] -> [a]
filterMulti [] xs = xs
filterMulti (f:fs) xs = filterMulti fs $ filter f xs

reverseApply :: (Int -> Int) -> Int -> Int
reverseApply f x = until ((x ==) . f) (+1) 0

generateList :: (Integer -> a) -> [a]
generateList f = map f [0..]