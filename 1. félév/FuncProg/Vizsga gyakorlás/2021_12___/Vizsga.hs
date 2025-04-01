module Vizsga where
import Data.Maybe
import Data.List
import Data.Char
import Data.Function

byDunaOrTisza :: String -> Bool
byDunaOrTisza ('D':'u':'n':'a':_) = True
byDunaOrTisza ('T':'i':'s':'z':'a':_) = True
byDunaOrTisza _ = False

howManyDoubles :: Eq a => [[a]] -> Int
howManyDoubles [] = 0
howManyDoubles ([a,b]:xs) = if a == b then 1 + howManyDoubles xs else howManyDoubles xs

blackJackPoints :: Integral a => [a] -> Maybe a
blackJackPoints xs
    | sum xs > 21 = Nothing
    | and $ map (<= 11) xs = Just $ sum xs
    | otherwise = Nothing

notDivisibleByThree:: Integral a => [a] -> Maybe Int
notDivisibleByThree [] = Nothing
notDivisibleByThree xs = if take thr xs == xs then Nothing else Just (thr + 1) where
    thr = length $ takeWhile (\x -> mod x 3 == 0) xs

crowd :: Int -> String
crowd 0 = ""
crowd x
    | odd x = (concat $ replicate (div x 2) "(-_") ++ "(-_-)" ++ (concat $ replicate (div x 2) "_-)")
    | otherwise = (concat $ replicate (div (x-1) 2) "(-_") ++ "(-_-)(-_-)" ++ (concat $ replicate (div (x-1) 2) "_-)")

atLeastNFrom :: Eq a => Int -> a -> [a] -> Bool
atLeastNFrom n _ [] = if n > 0 then False else True
atLeastNFrom n a (x:xs)
    | n <= 0 = True
    | a == x = atLeastNFrom (n-1) a xs
    | otherwise = atLeastNFrom n a xs

mapEither :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapEither _ _ _ [] = []
mapEither p f1 f2 (x:xs) = if p x then (f1 x) : mapEither p f1 f2 xs else (f2 x) : mapEither p f1 f2 xs 

numberOfFails :: Integral a => [[a]] -> Int
numberOfFails [] = 0
numberOfFails (x:xs)
    | genericLength x == 0 = numberOfFails xs
    | div (sum x) (genericLength x) < 2 =  1 + numberOfFails xs
    | otherwise = numberOfFails xs

encode :: String -> String
encode [] = []
encode (x:xs)
    | nextLength == 0 = x : encode xs
    | otherwise = x : show (nextLength + 1) ++ (encode $ dropWhile (==x) xs) where
        nextLength = length $ takeWhile (==x) xs

mergedOf :: Eq a => [a] -> [a] -> [a] -> Bool
mergedOf [] [] [] = True
mergedOf [] b c = b == c
mergedOf a [] c = a == c
mergedOf (a:as) (b:bs) (c1:c2:cs)
    | a == c1 && b == c2 = mergedOf as bs cs
    | otherwise = False
mergedOf _ _ _ = False

data Weather = Sunny | Cloudy | Rainy
    deriving(Show, Eq)

data Forecast = Prediction Weather Int
    deriving(Show, Eq)

summerVacation :: [Forecast] -> [Weather]
summerVacation = map getType . filterGood . groupGood where
    isGoodDay (Prediction Rainy _) = False
    isGoodDay _ = True
    groupGood = groupBy (\x y -> isGoodDay x && isGoodDay y)
    getMax xs = maximumBy (compare `on` length) xs
    getType (Prediction x _) = x
    filterGood = getMax . filter (\x -> case x of {[] -> False; (a:_) -> if isGoodDay a then True else False})

--summerVacation [Prediction Sunny 60, Prediction Cloudy 70] ==[Sunny,Cloudy]
--summerVacation [Prediction Rainy 30, Prediction Sunny 50,Prediction Cloudy 65] == [Sunny,Cloudy]
--summerVacation [Prediction Rainy 10, Prediction Sunny 49,Prediction Cloudy 57, Prediction Rainy 10, Prediction Sunny 49] ==[Sunny,Cloudy]
--summerVacation [Prediction Rainy 10, Prediction Sunny 49,Prediction Cloudy 57, Prediction Rainy 10, Prediction Cloudy 57,Prediction Sunny 49] == [Sunny,Cloudy]
--summerVacation [Prediction Rainy 10, Prediction Sunny 49,Prediction Cloudy 57, Prediction Rainy 10, Prediction Cloudy 57,Prediction Sunny 49, Prediction Sunny 49] == [Cloudy,Sunny,Sunny]
--summerVacation [Prediction Sunny 90, Prediction Sunny 49,Prediction Cloudy 57, Prediction Rainy 10, Prediction Cloudy 57,Prediction Sunny 49, Prediction Sunny 49] == [Sunny,Sunny,Cloudy]
--summerVacation [Prediction Rainy 10, Prediction Sunny 49,Prediction Cloudy 57, Prediction Rainy 10, Prediction Rainy 60,Prediction Rainy 40, Prediction Cloudy 57, Prediction Sunny 49,Prediction Sunny 49] == [Cloudy,Sunny,Sunny]
--summerVacation [Prediction Rainy 10, Prediction Sunny 49,Prediction Rainy 60, Prediction Cloudy 57, Prediction Rainy 10,Prediction Rainy 60, Prediction Rainy 40, Prediction Cloudy 57,Prediction Sunny 49, Prediction Sunny 49, Prediction Rainy 60,Prediction Rainy 76, Prediction Cloudy 60] == [Cloudy,Sunny,Sunny]

nthSmallest :: (Integral a, Eq b) => a -> [b] -> Maybe b
nthSmallest _ [] = Nothing
nthSmallest n (x:xs)
    | n < 1 = Nothing
    | n == 1 = Just x
    | otherwise = nthSmallest (n-1) $ dropWhile (==x) xs

highlight :: String -> String -> String
highlight [] xs = xs
highlight _ [] = []
highlight (h:hs) (x:xs)
    | check (h:hs) (x:xs) = toUpper h : (highlight (h:hs) (map (toUpper) hs ++ drop (length hs) xs))
    | otherwise  = x : highlight (h:hs) xs where
        check [] _ = True
        check _ [] = False
        check (h:hs) (x:xs) = if toLower h == toLower x then check hs xs else False

roughNumber :: Int -> Int -> Bool
roughNumber k n = and $ map (>= k) $ filter prime $ divisors n where
    divisors x = [m | m <- [2..(x-1)], mod x m == 0]
    prime x = foldr (\y acc -> if acc then mod x y /= 0 else False) True [2..(x-1)]