module Homework6 where
import Data.Char

hash :: String -> Integer
hash x = sum[(fromIntegral m)*(2^(ord n)) |(n,m) <- (zip x [1..(length x)])]

caesarEncrypt :: String -> Int -> String
caesarEncrypt x 0 = x
caesarEncrypt [] _ = []
caesarEncrypt x y = [chr((mod ((ord n) - 97 + y) 26) + 97) | n <- x]

listDiff :: Eq a => [a] -> [a] -> [a]
listDiff x y =  [n | n <- x, not(elem n y)]

validGame :: String -> Bool
validGame x = (length(words x) - 1) ==
              (length[i | i <- [0..(length (words x))-2], (last((words x)!!i)) == (head((words x)!!(i+1)))])