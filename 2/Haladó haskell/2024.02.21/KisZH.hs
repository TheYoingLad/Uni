{-# LANGUAGE InstanceSigs #-}
module KisZH01 where
f15 :: (a -> b, b -> c) -> Either a a -> c
f15 (f, g) (Right a) = g $ f a
f15 (f, g) (Left a) = g $ f a

data Tuple a = MkTuple a a Int

instance (Eq a) => Eq (Tuple a) where
    (==) (MkTuple a1 a2 n) (MkTuple b1 b2 m) = a1 == b1 && a2 == b2 && n == m