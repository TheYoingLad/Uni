module Orai7 where
import Data.Char

newtype Name = N String
    deriving (Eq, Ord, Show)

properName :: Name -> Bool
properName (N (x:xs)) = isUpper x

newtype Pont = P (Int, Int)
    deriving (Show, Eq)

data Pont' = P' Int Int
    deriving (Show, Eq)

data Binary = On | Off
    deriving (Show, Eq)

switch :: Binary -> Binary
switch On = Off
switch Off = On

bxor :: [Binary] -> [Binary] -> [Binary]
bxor [] [] = []
bxor (a:b) (c:d)
    | a == c = On:(bxor b d)
    | otherwise = Off:(bxor b d)

data Colour = RGB Int Int Int

red :: Colour
red = RGB 255 0 0
green :: Colour
green = RGB 0 255 0
blue :: Colour
blue = RGB 0 0 255

instance Show Colour where
    show (RGB r g b) = "R = " ++ show r ++ " G = " ++ show g ++ " B = " ++ show b
instance Eq Colour where
    (==) (RGB r1 g1 b1) (RGB r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

isGray :: Colour -> Bool
isGray (RGB 0 0 0) = False
isGray (RGB 255 255 255) = False
isGray (RGB r g b) = r == g && g == b

data Weather = Sunny | Cloudy | Rainy Int | Snowy Int
    deriving (Show, Eq)

niceDayForHiking :: Weather -> Bool
niceDayForHiking Sunny = True
niceDayForHiking Cloudy = True
niceDayForHiking _ = False

weatherForecastInaccuracy :: [Weather] -> [Weather] -> Int
weatherForecastInaccuracy [] _ = 0
weatherForecastInaccuracy _ [] = 0
weatherForecastInaccuracy ((Rainy _):a) ((Rainy _):b) = weatherForecastInaccuracy a b
weatherForecastInaccuracy ((Snowy _):a) ((Snowy _):b) = weatherForecastInaccuracy a b
weatherForecastInaccuracy (a:b) (c:d)
    | a /= c = 1 + weatherForecastInaccuracy b d
    | otherwise = weatherForecastInaccuracy b d

data Temperature = Night Double | Daytime Double
    deriving (Show, Eq)

isDaytime :: Temperature -> Bool
isDaytime (Daytime _) = True
isDaytime _ = False

daytimeAvg :: [Temperature] -> Double
daytimeAvg [] = 0.0
daytimeAvg x = (sum y) / (fromIntegral(length y)) where
    y = [n | (Daytime n) <- x]