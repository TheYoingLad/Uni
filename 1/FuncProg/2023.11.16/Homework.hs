module Homework where
import Data.Char

upperCharToLower :: String -> String
upperCharToLower = map toLower . filter isUpper . filter isAlpha

swapIfCond :: (a -> Bool) -> [(a,a)] -> [(a,a)]
swapIfCond f = map (\(x,y) -> if (f x) then (y,x) else (x,y))

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f = fst . unzip . filter (snd) . zip [0..] . map f