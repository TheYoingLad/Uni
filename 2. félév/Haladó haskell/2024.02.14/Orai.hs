{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Gy01 where

xor :: Bool -> Bool -> Bool
xor x y = x /= y

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (_,(b,(c,_))) = (b,c)

f2 :: (a -> b) -> a -> b
f2 f = f

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g = f . g

f4 :: (a -> b -> c) -> b -> a -> c
f4 f = flip f

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 f a b = f (a,b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a,b) = f a b

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst $ f a,\a -> snd $ f a)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) = \a -> (f a, g a)

f9 :: Either a b -> Either b a
f9 (Left a) = Right a
f9 (Right b) = Left b

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\a -> f $ Left a, \b -> f $ Right b)

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f, g) = \a -> case a of {Left a -> f a; Right a -> g a}

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Right (a,c)) = (a, Right c)
f12 (Left (a,b)) = (a, Left b)

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (a, Left b) = Left (a,b)
f13 (a, Right c) = Right (a,c)

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = f (g (\a -> f a a)) (g (\a -> f a a))
 
data Colour = RGB Int Int Int

data List a = Nil | Cons a (List a)

data Tree a = EmptyTree | TreeCons a (Tree a) (Tree a)

instance Eq Colour where
    (==) (RGB a b c) (RGB x y z) = (a == x) && (b == y) && (c == z)

instance Show Colour where
    show (RGB a b c) = show a ++ show b ++ show c

instance (Eq a) => Eq (List a) where
    (==) Nil Nil = True
    (==) (Cons x xs) (Cons y ys) = x == y && xs == ys
    (==) _ _ = False 

instance (Show a) => Show (List a) where
    show Nil = "[]"
    show (Cons x xs) = "[" ++ show x ++ "," ++ (drop 1 $ show xs)

instance (Eq a) => Eq (Tree a) where
    (==) EmptyTree EmptyTree = True
    (==) (TreeCons x x1 x2) (TreeCons y y1 y2) = x == y && x1 == y1 && x2 == y2
    (==) _ _ = False