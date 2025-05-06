module Homework5 where

toBin :: Integer -> [Int]
toBin 0 = []
toBin x 
    | mod x 2 == 0 = 0:(toBin(div x 2)) 
    | otherwise = 1:(toBin(div x 2))

remdup :: Eq a => [a] -> [a]
remdup [] =  []
remdup [a] = [a]
remdup (a:b:c)
    | a /= b = a:(remdup(b:c))
remdup (_:a) = remdup a

keepIncreasingTriples :: Ord a => [(a,a,a)] -> [(a,a,a)]
keepIncreasingTriples [] = []
keepIncreasingTriples ((a,b,c):x)
    | a < b && b < c = (a,b,c):(keepIncreasingTriples x)
    | otherwise = keepIncreasingTriples x

deleteEveryThird :: [a] -> [a]
deleteEveryThird [] = []
deleteEveryThird (a:b:c:d) = a:b:(deleteEveryThird d)
deleteEveryThird a = a

alternate :: [a] -> [a] -> [a]
alternate [] [] = []
alternate (a:b) (c:d) = a:(alternate d b)