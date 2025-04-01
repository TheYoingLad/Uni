module Homework where
import Data.Maybe
import Data.List

data T = A String Int | B String Int | C

f :: [T] -> Int
f (_:_:B _ 1:_)            = 0
f (_:_:B [] _:[])          = 1
f (_:A [_] _:B [] _:[xs])  = 2
f (_:A _ _:B _ _:xs)       = 3

g = [C, A "a" 1, B "" 2, C]

triangleArea :: (Double, Double, Double) -> Maybe Double
triangleArea (x, y, z)
    | x <= 0 || y <= 0 || z <= 0 = Nothing
    | x+y <= z || x+z <= y || y+z <= x = Nothing
    | x^2 + y^2 == z^2 = Just ((x*y)/2)
    | otherwise = Nothing

data Storage = HDD String Int Int | SSD String Int
    deriving(Show, Eq)

instance Ord Storage where
    (SSD _ x1) `compare` (SSD _ x2) = x1 `compare` x2
    (HDD _ _ x1) `compare` (SSD _ x2) = x1 `compare` x2
    (HDD _ _ x1) `compare` (HDD _ _ x2) = x1 `compare` x2

largestSSD :: [Storage] -> Maybe Storage
largestSSD x = maximumStorage[n | n@(SSD _ _) <- x] where
    maximumStorage [] = Nothing
    maximumStorage x = Just (maximum x)

hugeHDDs :: [Storage] -> [Storage]
hugeHDDs x = [n | n@(HDD _ _ _) <- x, n > actual(largestSSD x)] where
    actual Nothing = HDD "" 0 0
    actual (Just x) = x