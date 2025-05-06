module Orai12 where

data Stack a = Empty | S a (Stack a)

instance (Show a) => Show (Stack a) where
    show Empty = "{}"
    show stack = "{" ++ inside stack ++ "}" where
        inside (S x Empty) = show x
        inside (S x rest) = show x ++ ";" ++ inside rest

instance (Eq a) => Eq (Stack a) where
    (==) Empty Empty = True
    (==) (S x xs) (S y ys) = x == y && xs == ys
    (==) _ _ = False

top :: Stack a -> a
top (S x xs) = x
top _ = error "empty stack"

push :: a -> Stack a -> Stack a
push x xs = S x xs

pop :: Stack a -> Stack a
pop (S x xs) = xs
pop _ = error "empty stack"

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

listToStack :: [a] -> Stack a
listToStack [] = Empty 
listToStack (x:xs) = S x (listToStack xs)

popMult :: Int -> Stack a -> Stack a
popMult 0 xs = xs
popMult _ Empty = Empty
popMult n (S _ xs) = popMult (n-1) xs

popWhile :: (a -> Bool) -> Stack a -> Stack a
popWhile _ Empty = Empty
popWhile p (S x xs)
    | p x = popWhile p xs
    | otherwise = (S x xs)

reverseStack :: Stack a -> Stack a
reverseStack = listToStack . reverse . stackToList

stackToList :: Stack a -> [a]
stackToList Empty = []
stackToList (S x xs) = x:(stackToList xs)

wellFormed :: String -> Bool
wellFormed xs = kibe Empty $ listToStack xs where
    kibe stack Empty = isEmpty stack
    kibe stack (S x xs)
        | x == ')' = if stack == Empty then False else kibe (pop stack) xs
        | x == '(' = kibe (push 1 stack) xs