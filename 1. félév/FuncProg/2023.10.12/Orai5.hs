module Orai5 where

g (_:([],(_:x)):[_]) = "ok"
f = [([1..10],[2..5]),([],[4]),([],[])]

hasEmpty :: [[a]] -> Bool
hasEmpty [] = False
hasEmpty (a:b)
    | null a = True
    | otherwise = hasEmpty b

length3lists :: [[a]] -> [[a]]
--length3lists x = [n | n@[_,_,_]<-x]
length3lists [] = []
length3lists (a@[_,_,_]:b) = a:(length3lists b)
length3lists (_:a) = length3lists a

findWord :: [String] -> String
findWord [] = error "nincs ilyen szo"
findWord (a@(x:_:y:_):b)
    | x == y = a
findWord (_:a) = findWord a

duplicateElements :: [a] -> [a]
duplicateElements [] = []
duplicateElements (a:b) = a:a:(duplicateElements b)

everySecond :: [a] -> [a]
everySecond [] = []
everySecond (_:b:c) = b:(everySecond c)
everySecond [a] = []

pizza :: [(String, Int)] -> Int
pizza [] = 0
pizza [(_,x)] = 500 + x
pizza ((_,x):a) = x + (pizza a)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' x (a:b) = a:(take (x-1) b)

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' x (a:b) = drop (x-1) b

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a:b) (c:d) = a == c && isPrefixOf b d

sortTuples :: Ord a => [(a,a)] -> [(a,a)] 
sortTuples [] = []
sortTuples ((a,b):x)
    | a > b = (b,a):(sortTuples x)
    | otherwise = (a,b):(sortTuples x)

keepIncreasingTriples :: Ord a => [(a,a,a)] -> [(a,a,a)]
keepIncreasingTriples [] = []
keepIncreasingTriples ((a,b,c):x)
    | a<b && b<c = (a,b,c):(keepIncreasingTriples x)
    | otherwise = keepIncreasingTriples x

deleteEveryThird :: [a] -> [a]
deleteEveryThird [] = []
deleteEveryThird (a:b:c:d) = a:b:(deleteEveryThird d)
deleteEveryThird a = a

alternate :: [a] -> [a] -> [a]
alternate [] [] = []
alternate (a:b) (c:d) = a:(alternate d b)