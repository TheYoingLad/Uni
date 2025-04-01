module Homework4 where

password :: [Char] -> [Char]
password [] = []
--password x = [n | i <- [0..((length x)-1)], n <- "*"]
password [a] = "*"
password (a:b) = "*" ++ (password b)

filterIncPairs :: Ord a => [(a,a)] -> [(a,a)]
filterIncPairs [] = []
--filterIncPairs x = [n | i <- [0..((length x)-1)], n <- x, ((fst(unzip x))!!i) < ((snd(unzip x))!!i), x!!i == n]
--filterIncPairs x = [n | (a, b)<-x, a < b]
filterIncPairs [a]
    | (fst a) < (snd a) = [a]
    | otherwise = []
filterIncPairs (a:b) = (filterIncPairs [a]):(filterIncPairs b)

headTail :: [a] -> (a, [a])
headTail x = ((head x), (tail x))

doubleHead :: [a] -> [b] -> (a, b)
doubleHead x y = ((head x),(head y))

changeHead :: [a] -> a -> [a]
changeHead [] y = [y]
changeHead (a:b) y = (y:b)


----------------------------------------EXTRA---------------------------------


lista = [n*m | n <- [1,3..], m <- [1, (-1)], ((m == 1)&&(mod n 4 == 1))||((m == (-1))&&(mod n 4 == 3))]

reciprok :: (Integral a, Fractional b) => [a] -> [b]
reciprok [a] = [(1/(fromIntegral a))]
reciprok (a:b) = [(1/(fromIntegral a))] ++ (reciprok b)

pi' :: Floating a => a
pi' = 4*(sum(take 1000 (reciprok lista)))

combinationsBool3 :: [[Bool]]
combinationsBool3 = [[(mod n 8 > 3), (mod n 4 > 1), (mod n 2 > 0)] | n <- [0..7]]

combinationsBool :: Int -> [[Bool]]
combinationsBool 0 = [[]]
combinationsBool x = [(n:ns) | n <-[True, False], ns <- (combinationsBool (x-1))]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations x y = [(n:ns) | n <-y, ns <- (combinations (x-1) y)]