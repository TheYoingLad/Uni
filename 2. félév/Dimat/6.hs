module Beadhato6 where
import Data.List

------------------- 1. feladat -------------------------

toPatV :: String -> (Int, Int)
toPatV = foldr (\x (a,b) -> if x == 'a' then (a+1,b) else (a,b+1)) (0,0)

toMS :: [(Int, Int)] -> [((Int, Int), Int)]
toMS = foldr (\x acc -> case lookup x acc of {Nothing -> (x, 1):acc; (Just n) -> (x, n+1):(delete (x, n) acc)}) []

substring :: String -> [String]
substring s = [sub | n <- [0..(length s)-1], m <- [1..(length s)-n], let sub = take m (drop n s)]

test :: String -> [((Int, Int), Int)]
test = sort . toMS . map toPatV . filter (/= []) . sortBy (\a b -> compare (length a) (length b)) . substring

alapok :: Int -> [String]
alapok n
    | n <= 0 = []
    | n == 1 = ["a", "b"]
    | otherwise = (map ('a':) $ alapok (n-1)) ++ (map ('b':) $ alapok (n-1))

feladat1 = [(a,b) | n <- [0..8] , a <- alapok n, b <- alapok n, test a == test b, a /= b, a /= reverse b]
--ez a fgv ellenőrzi 0-8 hosszú minden s sorozatokra, hogy igaz-e a feltétel
--persze csak azonos hosszúságúakat mér össze, máskülönben biztos nem egyeznének meg a multihalmazok ((0,1) és (1,0) eltérés miatt pl)

--teszteléskor megnéztem 10ig, de a kód sok gondolkozás után nem talált semmit

{- Megoldások (amiket talált a kód)

abaabbab == abbabaab
abaabbab == baababba
baababba == babbaaba
abbabaab == babbaaba

nem meglepő, de ennek a 4 sorozatnak Patrikh-multihalmaza mind ugyan az
-}

------------------- 2. feladat -------------------------

--polinom összeadás és szorzás:
instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

poli = [1, 1, 1, 1, 1, 1] -- [1/6, 1/6, 1/6, 1/6, 1/6, 1/6]
generator = (poli^17)!!(50-17)

aladar = poli^50
bela = poli^40

feladat2 = sum[a' | (i, a) <- (zip [0..] aladar), let bela' = take (10+i) bela, let a' = a * sum bela'] / (6^(40+50)) --ha a polinom 1/6-ból áll, akkor nem kell a végén leosztani

--tehát kb 98,3% eséllyel dob Aladár nagyobbat, azaz gyakorlatilag garantáltan :)