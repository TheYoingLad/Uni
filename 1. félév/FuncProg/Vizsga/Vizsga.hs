module Vizsga where
import Data.Char
import Data.List

-------------   B csoport --------------
-- Golyha Gergő
-- A7MMZ1
-- 2024.01.12

sparsePairs :: [a] -> [(a,a)]
sparsePairs (x1:x2:x3:xs) = (x1,x2):sparsePairs xs
sparsePairs (x1:x2:xs) = (x1,x2):sparsePairs xs
sparsePairs _ = []

parentheses :: String -> Bool
parentheses xs
    | all (flip elem "()") xs = (foldr (\x acc -> if x == '(' then (acc - 1) else (acc + 1)) 0 xs) == 0
    | otherwise = False

checkRepetition :: String -> Int
checkRepetition [] = 0
checkRepetition xs = comp xs1 xs2 where
    xs1 = takeWhile (not . isSpace) xs
    xs2 = tail $ dropWhile (not . isSpace) xs
    comp [] [] = 0
    comp (x1:xs1) (x2:xs2)
        | x1 == x2 = comp xs1 xs2
        | otherwise = 1 + (comp xs1 xs2)

filterXor :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterXor _ _ [] = [] 
filterXor p1 p2 (x:xs)
    | (p1 x) && (not $ p2 x) = x:(filterXor p1 p2 xs)
    | (not $ p1 x) && (p2 x) = x:(filterXor p1 p2 xs)
    | otherwise = filterXor p1 p2 xs

stagnatingPrefix :: Eq b => (a -> b) -> [a] -> [a]
stagnatingPrefix _ [] = []
stagnatingPrefix _ [x] = [x]
stagnatingPrefix f (x1:x2:xs)
    | f x1 == f x2 = x1:stagnatingPrefix f (x2:xs)
    | otherwise = [x1]
    
data Colour = Brown | LightBlue | Pink | Orange |Red |Green | DarkBlue | Yellow
    deriving(Show, Eq)

type Price = Int

data Property = Site String Price Colour | Station String Price | Utility String Price
    deriving (Eq)

instance Show Property where
    show (Site x y _) = x ++ " ($" ++ show y ++ ")"
    show (Station x y) = x ++ " ($" ++ show y ++ ")"
    show (Utility x y) = x ++ " ($" ++ show y ++ ")"

showTest :: Property -> String
showTest pr = show pr

matchStartEnd :: Eq a => [a] {-mi-} -> [a] {-miben-} -> Maybe (Int, Int)
matchStartEnd [] _ = Just (0,0)
matchStartEnd _ [] = Nothing
matchStartEnd xs a@(y:ys)
    | isPrefixOf xs a = Just (0, (length xs)-1)
    | otherwise = plusOne(matchStartEnd xs ys) where
        plusOne (Just (a,b)) = Just (a+1, b+1)
        plusOne Nothing = Nothing

altParityChain :: String -> String
altParityChain [] = []
altParityChain xs = unwords $ filterTwo $ words xs where
    filterTwo [] = []
    filterTwo [x] = [x]
    filterTwo (x1:x2:xs)
        | (mod (length x1) 2) == (mod (length x2) 2) = filterTwo (x1:xs)
        | otherwise = x1:filterTwo (x2:xs) 