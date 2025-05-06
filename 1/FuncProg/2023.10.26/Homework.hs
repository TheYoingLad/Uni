module Homework where

data RPS = Rock | Paper | Scissors
    deriving (Show, Eq)

firstPlayerWon :: [(RPS, RPS)] -> Int
firstPlayerWon x = sum[1 | (a,b) <- x, a == Rock && b == Scissors || a == Paper && b == Rock || a == Scissors && b == Paper]

data Vector3 = V Int Int Int
    deriving (Eq, Show)

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (V a1 a2 a3) (V b1 b2 b3) = V (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

vectorListSum :: [Vector3] -> Vector3
vectorListSum x = V (sum[n | (V n _ _) <- x]) (sum[n | (V _ n _) <- x]) (sum[n | (V _ _ n) <- x])

data Temperature = Night Int | Daytime Int
    deriving (Show, Eq)

extremes :: [Temperature] -> (Int, Int)
extremes x = (maximum[n | (Daytime n) <- x], minimum[n | (Night n) <- x])