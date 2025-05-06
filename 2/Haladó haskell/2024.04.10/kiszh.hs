{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveFoldable, QuantifiedConstraints #-}

module Kiszh where

data CrazyType3 f a b = C1 (f (f a)) (f b) | C2 b a b Int | C3 (CrazyType3 f a b) a deriving (Functor, Foldable)

deriving instance (Eq a, Eq b, forall a. Eq a => Eq (f a)) => Eq (CrazyType3 f a b)

instance (Traversable fun) => Traversable (CrazyType3 fun fixed) where
    traverse :: (Applicative f, Traversable fun) => (a -> f b) -> CrazyType3 fun fixed a -> f (CrazyType3 fun fixed b)
    traverse f (C1 funfunfixed funa) = C1 <$> pure funfunfixed <*> traverse f funa
    traverse f (C2 a1 fixed a2 n) = C2 <$> f a1 <*> pure fixed <*> f a2 <*> pure n
    traverse f (C3 as fixed) = C3 <$> traverse f as <*> pure fixed