module Orai2 where

two :: Integer -- nagyobb pozitív szám, vizsga esetén Integer
two = 2

-- curryzés
-- fgv defelés fgv-vel

inc :: Int -> Int
inc = (1+)--(+1) is lehet mert kommutatív
    -- ^ szelet/részleges fgv alkalmazás, mert nem kapja meg az összes paramétert

double :: Int -> Int
double = (2*) --(*2) is lehet mert kommutatív

add :: Int -> (Int -> Int)
add = (+)

-------------------------------------
-- mintaillesztés
-------------------------------------

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

isNull :: Integer -> Bool
isNull 0 = True
isNull _ = False

-- term számokra
lowerThan5 :: Int -> Int
lowerThan5 x
    | x < 5 = 5
    | otherwise = x

lowerThan5' :: Int -> Int
lowerThan5' 0 = (0+3) `div` 2
lowerThan5' 1 = (1+3) `div` 2
lowerThan5' 2 = (2+3) `div` 2
lowerThan5' 3 = (3+3) `div` 2
lowerThan5' 4 = (4+3) `div` 2
lowerThan5' x = (x + 3) `div` 2

lowerThan5'' :: Int -> Int
lowerThan5'' x = if x < 5 then 5 else x -- ezt nem szeretjük!

-------------------------------------
-- tuple = több típusú
-------------------------------------
-- pl (1, "alma", True)

--  rendezett n-es
fst' :: (Int, Int) -> Int
fst' (x, y) = x

snd' :: (Int, Int) -> Int
snd' (x, y) = y

-- parametrikus poliporfizmus
fst'' :: (a,b) -> a -- ha az 'a' és 'b' különböző típus
fst'' (x, y) = x

swap :: (a,b) ->(b, a)
swap (x, y) = (,) y x

swap' :: (a,b) ->(b, a)
swap' (x,y) = (y ,x)

swap'' :: (a,b) ->(b, a)
swap'' x = (snd x , fst x)

triplicate :: a -> (a, a, a)
triplicate x = (x, x, x)

isMatching :: (Int, Int) -> (Int, Int) ->Bool
isMatching x y = fst x == fst y || fst x == snd y || snd x == fst y || snd x == snd y

doubleTouple :: (a, b) -> ((a,b),(a,b))
doubleTouple x = (x, x)

doubleTouple' :: (a, b) -> ((a,b),(a,b))
doubleTouple' x@(n,m) = (x, x)
            -- ^ alias
