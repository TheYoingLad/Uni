module Orai8 where

data Peksuti = Kifli String | Zsemle String Bool | KakaosCsiga Int
f :: [Peksuti] -> Int
f (_:_:Zsemle _ True:_)             = 0
f (_:_:Zsemle [] _:[])              = 1
f (_:Kifli [_]:Zsemle [] _:[xs])    = 2
f (_:Kifli _:Zsemle _ _:xs)         = 3

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv a b = Just (div a b)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd (Just a) (Just b) = Just (a + b)
maybeAdd _ _ = Nothing

addBefore :: Maybe a -> [a] -> [a]
addBefore Nothing x = x
addBefore (Just x) y = x:y

countNothings :: [Maybe a] -> Int
countNothings x = sum[1 | Nothing <- x]

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex x y
    | y < 0 = Nothing
    | y > length x = Nothing
    | otherwise = Just (x!!y)

data Point = Point Double Double
    deriving(Show, Eq)

getx :: Point -> Double
getx (Point x _) = x
gety :: Point -> Double
gety (Point _ y) = y

displace :: Point -> Point -> Point
displace (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

data Shape = Circle Point Double| Rect Point Point
    deriving (Show, Eq)

area :: Shape -> Double
area (Circle _ r) = r^2 * pi
area (Rect (Point x1 y1) (Point x2 y2)) = abs(x2-x1) * abs(y2-y1)

displaceShape :: Shape -> Point -> Shape
displaceShape (Circle x r) y = Circle (displace x y) r
displaceShape (Rect x y) z = Rect (displace x z) (displace y z)