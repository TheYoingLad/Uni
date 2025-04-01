module Homework where

--számok listái

numbers1 :: [Int]
numbers1 = [n | n<-[1000,999..0], mod n 5 == 3, mod (3 * n) 7 == 2]

numbers2 :: [Int]
numbers2 = [div n 2 | n<-[0..500], mod n 7 == 3, mod (2 * n) 6 == 2, mod n 2 == 0]

numbers3 :: [Int]
numbers3 = [n | n<-[1..100], (mod n 3 == 0 || mod n 5 == 0) && mod n 15 /= 0, mod n 2 == 0]


--barátságos számok

osztok :: Int -> [Int]
osztok x = [n | n<-[1..(x-1)], mod x n == 0]

areAmicableNumbers :: Int -> Int -> Bool
areAmicableNumbers x y = sum(osztok x) == y && sum(osztok y) == x