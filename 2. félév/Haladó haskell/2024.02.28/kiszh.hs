module Kiszh where
 
data CrazyType3 f a b = C1 (f a) (f b) | C2 a b b | C3 (CrazyType3 f a b) b deriving Eq

instance (Functor fixed1) => Functor (CrazyType3 fixed1 fixed2) where
    fmap ::(a -> b) -> CrazyType3 fixed1 fixed2 a -> CrazyType3 fixed1 fixed2 b
    fmap f (C1 a1 a2) = C1 a1 (fmap f a2)
    fmap f (C2 fixed a1 a2) = C2 fixed (f a1) (f a2)
    fmap f (C3 as a) = C3 (fmap f as) (f a)