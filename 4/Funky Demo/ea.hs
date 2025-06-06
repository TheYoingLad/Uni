module EA where

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

looop :: a -> a
looop = looop

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

fact' :: Int -> Int
fact' n = belso n 1
  where
    belso 1 acc = acc
    belso n acc = belso (n - 1) (n * acc)

fib' :: Int -> Int
fib' n = belso2 n 0 1 where
    belso2 1 acc1 acc2 = acc1
    belso2 n acc1 acc2 = belso2 (n-1) (acc2) (acc1 + acc2)