module Homework1 where

int1 :: Int
int1 = 1
int2 :: Int
int2 = 2
int3 :: Int
int3 = 3

bool1 :: Bool
bool1 = True || False
bool2 :: Bool
bool2 = False && False
bool3 :: Bool
bool3 = True == True
-- elnézést a félreértésért, egyszer ezt már átírtam de ezek szerint nem mentettem el!


double :: Int -> Int
double x = x * 2

inc :: Int -> Int
inc x = x + 1

seven1 :: Int
seven2 :: Int
seven3 :: Int

seven1 = inc (inc (inc (inc  (inc (inc (inc 0))))))
seven2 = inc (double (inc (double (inc 0))))
seven3 = inc (inc (inc (double (double (inc 0)))))

isLuckyNumber :: Integer -> Bool
isLuckyNumber x
    | mod x 13 == 0 && mod x 2 == 1 && x /= 13 = True
    | otherwise = False

isLeapYear :: Integer -> Bool
isLeapYear x
    | mod x 400 == 0 = True
    | mod x 4 == 0 && mod x 100 /= 0 = True
    | otherwise = False