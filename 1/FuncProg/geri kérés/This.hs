module This where

prepare :: [(a -> (Bool, a))] -> a -> a
prepare [] x = x
prepare (f:xs) x
    | fst(f x) = prepare xs y
    | otherwise = prepare xs x where
        y = snd(f x)

stabilize :: Eq a => (a -> a) -> a -> a
stabilize f x
    | x == f x = x
    | otherwise = stabilize f $ f x

brew :: (Integral a, Num b) => (b -> Bool) -> (b -> b) -> [[a]] -> b
brew f g xs = brewHelp f g xs 0 where
    brewHelp :: (Integral a, Num b) => (b -> Bool) -> (b -> b) -> [[a]] -> b -> b
    brewHelp _ _ [] acc = acc
    brewHelp f g (x:xs) acc
        | f ((sum (map fromIntegral x)) + acc) = brewHelp f g xs ((sum (map fromIntegral x)) + acc)
        | otherwise = brewHelp f g (x:xs) (g acc)

cooldown :: Eq a => [a] -> (Int -> Bool) -> [a]
cooldown [] _ = []
cooldown a@(x:xs) f
    | f (length $ takeWhile (==x) a) = x:(cooldown (dropWhile (==x) a) f)
    | otherwise = (takeWhile (==x) a) ++ (cooldown (dropWhile (==x) a) f)