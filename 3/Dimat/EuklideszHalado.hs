module Euklidesz where
import Data.List

gcdList :: Int -> Int -> [Int]
gcdList a b 
    | a == 0 || b == 0 = undefined
    | otherwise = reverse $ case a > b of
        True -> recList [b,a]
        False -> recList [a,b]
    where
    recList :: [Int] -> [Int]
    recList xs@(a:b:_) = case mod b a of
        0 -> 0:xs
        r -> recList (r:xs)
    
divList :: [Int] -> [Int]
divList (a:b:xs) = case b of
    0 -> []
    otherwise -> (div a b):(divList (b:xs))

getNext :: [Int] -> [Int] -> [Int]
getNext _ [] = []
getNext [b,c] (a:ys) = (c-a*b):(getNext [(c-a*b), b] ys)

getTable :: Int -> Int -> [(Int, Int, Maybe Int, Int)]
getTable a b = zip4 gcds (1:0:(getNext [0,1] divs)) ((Nothing):(map (Just) divs)++[Nothing]) (0:1:(getNext [1,0] divs)) where
    gcds = gcdList a b
    divs = divList gcds

concathelper :: String -> String -> String -> String -> String
concathelper a b c d = concat [a, "\t", b, "\t", c, "\t", d]

prettyPrint :: [(Int, Int, Maybe Int, Int)] -> IO ()
prettyPrint [] = return ()
prettyPrint ((a,b,c,d):xs) = do
    case c of
        Just c' -> do
            putStrLn $ concathelper (show a) (show b) (show c') (show d)
            prettyPrint xs
        Nothing -> do
            putStrLn $ concathelper (show a) (show b) "x" (show d)
            prettyPrint xs

bovitettEuklidesz :: Int -> Int -> IO ()
bovitettEuklidesz a b = prettyPrint $ getTable a b 