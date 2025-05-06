module Idea where

funkyTranspose :: [[Int]] -> Int -> [[Int]]
funkyTranspose mx n = take n $ map (take n) $ mxRev mx where
    mxRev :: [[Int]] -> [[Int]]
    mxRev mx = reverse $ map reverse mx

test :: [[Int]]
test = [[1,2,3],[4,5,6],[7,8,9]]

testPrint :: [[Int]] -> IO ()
testPrint [] = return ()
testPrint (line : mx) = do
    putStrLn $ show line
    testPrint mx

levelN :: Int -> [a] -> [a -> Bool] -> [a]
levelN level as preds = [elem | elem <- as, (sum $ map ((\x -> if x then 1 else 0) . (\p -> p elem)) preds) == level]