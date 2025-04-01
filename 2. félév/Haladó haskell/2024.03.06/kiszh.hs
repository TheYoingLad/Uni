module Kiszh where
 
data CrazyType3 f a b = C1 (f a) (f b) | C2 a b b | C3 (CrazyType3 f a b) b deriving Eq

instance (Foldable fixed1) => Foldable (CrazyType3 fixed1 fixed2) where
    foldMap f (C1 fixed fb) = foldMap f fb
    foldMap f (C2 fixed a1 a2) = f a1 <> f a2
    foldMap f (C3 as a) = foldMap f as <> f a