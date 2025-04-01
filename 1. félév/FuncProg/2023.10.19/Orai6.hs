module Orai6 where
import Data.Char

isLonger :: [a] -> [a] -> Bool
isLonger [] _ = False
isLonger _ [] = True
isLonger (a:b) (c:d) = isLonger b d

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:b) (c:d) = (a,c):(zip' b d)

toUpperFirsts :: String -> String
toUpperFirsts x = unwords [(toUpper n):m | (n:m) <- (words x)]

cipher :: String -> String
cipher [] = []
cipher (a:b:c:d)
    | (isLetter a) && (isLetter b) && (isDigit c) = [a,b]
    | otherwise = cipher (b:c:d)
cipher _ = []

identifier :: String -> Bool
identifier [] = False
identifier (a:b) = (isLetter a) && (correct b) where
    correct [] = True
    correct (x:y) = ((isLetter x) || (isDigit x) || (x == '_')) && (correct y)

hash :: String -> Integer
hash x = sum[(fromIntegral m)*(2^(ord n)) |(n,m) <- (zip x [1..(length x)])]

caesarEncrypt :: String -> Int -> String
caesarEncrypt x 0 = x
caesarEncrypt [] _ = []
caesarEncrypt x y = [chr((mod ((ord n) - 97 + y) 26) + 97) | n <- x]

---------------------------- BÓNUSZ -----------------------

keys :: [String]
keys = [concat[a, b, "-OEM-00", c, "-", d] |
        xa<-[0..3], ya<-[0..9], za<-[0..9], xa*100+ya*10+za>0, xa*100+ya*10+za<367, a<-[concat[show xa, show ya, show za]],
        b<-["95","96","97","98","99","00","01","02","03"],
        vc<-[0..9], wc<-[0..9], xc<-[0..9], yc<-[0..9], zc<-[0..9], mod (vc+wc+xc+yc+zc) 7 == 0, c<-[concat[show vc, show wc, show xc, show yc, show zc]],
        vd<-[0..9], wd<-[0..9], xd<-[0..9], yd<-[0..9], zd<-[0..9], d<-[concat[show vd, show wd, show xd, show yd, show zd]]]

validate :: String -> Bool
--validate x = elem x keys
validate x = a < 367 && a > 0 && elem b ["95","96","97","98","99","00","01","02","03"] &&
             mod c 7 == 0 && d where
    a = (digitToInt(x!!0))*100 + (digitToInt(x!!1))*10 + (digitToInt(x!!2))
    b = concat[show(digitToInt(x!!3)), show(digitToInt(x!!4))]
    c = digitToInt(x!!12) + digitToInt(x!!13) + digitToInt(x!!14) + digitToInt(x!!15) + digitToInt(x!!16)
    d = (take 7 (drop 5 x))=="-OEM-00" && x!!17=='-' && isDigit(x!!18)&&isDigit(x!!19)&&isDigit(x!!20)&&isDigit(x!!21)&&isDigit(x!!22)

keysN :: Int
keysN = 366*9*a*(10^5) where
    a = length[0 | v<-[0..9], w<-[0..9], x<-[0..9], y<-[0..9], z<-[0..9], mod (v+w+x+y+z) 7 == 0]