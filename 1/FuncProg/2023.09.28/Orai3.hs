module Orai3 where

--ad hoc polimorfizmus

isNull :: (Eq a, Num a) => a -> Bool
isNull 0 = True
isNull _ = False

add3 :: Num a => a -> a -> a -> a
add3 a b c = a+b+c

isEven :: (Eq a, Integral a) => a -> Bool
isEven x = mod x 2 == 0


--listák
divisors :: Int -> [Int]
divisors n = [x | x<-[1..n], mod n x == 0]