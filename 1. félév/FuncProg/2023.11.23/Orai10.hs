module Orai10 where
import Data.Char


c :: (a -> b -> c) -> (b -> a -> c)
c f x y = f y x

w :: (a -> a -> a) -> a -> a
w f x = f x x

mapFuns :: a -> [(a -> b)] -> [b]
mapFuns x = map ($ x)

ntimes :: (a -> a -> a) -> a -> Int -> a
ntimes f x 1 = x
ntimes f x n = f x $ ntimes f x (n - 1)

selectUnless :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
selectUnless p1 p2 = filter (not . p2) . filter p1

selectiveApply :: (a -> a) -> (a -> Bool) -> [a] -> [a]
selectiveApply f p1 = map (\x -> if p1 $ x then f $ x else x)

applyWithDefault :: (a -> Bool) -> (a -> b) -> b -> [a] -> [b]
applyWithDefault p1 f b = map (\x -> if p1 $ x then f $ x else b)

isUniform :: Eq b => (a -> b) -> [a] -> Bool
isUniform f [] = True
isUniform f (x:xs) = all ((==) $ f $ x) $ map f (x:xs)

insertWithPredicate :: (a -> a -> Bool) -> a -> [a] -> [a]
insertWithPredicate p1 a xs = takeWhile (not . p1 a) xs ++ [a] ++ dropWhile (not . p1 a) xs