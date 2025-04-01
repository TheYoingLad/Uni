{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving #-}
module Gy06 where

import Prelude hiding (Maybe(..), Either(..))

-- Definiáljuk egy függvényt, amely egy "mellékhatásos" függvényt végig mappol egy listán
--                                          v az m mellékhatást összegyűjtjük
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList _ [] = return []
mapMList f (x:xs) = do
  xs' <- mapMList f xs
  x' <- f x
  return ((x'):xs')

-- Mivel a Functor (sima mappolás) általánosítható volt, ez a mellékhatásos mappolás is lehet általánosítható
data Single a = Single a deriving (Eq, Show, Functor, Foldable)
data Tuple a = Tuple a a deriving (Eq, Show, Functor, Foldable)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show, Functor, Foldable)
data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor, Foldable)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor, Foldable)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor, Foldable)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show, Functor, Foldable)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Functor, Foldable)
data Either e a = Left e | Right a deriving (Eq, Show, Functor, Foldable)
data BiTuple e a = BiTuple e a deriving (Eq, Show, Functor, Foldable)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show, Functor, Foldable)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show, Functor, Foldable)
data Apply f a = MkApply (f a) deriving (Eq, Show, Functor, Foldable)
data Fix f a = MkFix (f (Fix f a)) deriving (Functor, Foldable)
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show, Functor, Foldable)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show, Functor, Foldable)
data FList f a = FNil | FCons (f a) (f (FList f a)) deriving (Functor, Foldable)

-- Írjuk meg ezt a műveletet pár fenti típusra!
mapMSingle :: Monad m => (a -> m b) -> Single a -> m (Single b)
mapMSingle f (Single a) = do
  a' <- f a
  return (Single a')

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f (Tuple a1 a2) = do
  a1'<- f a1
  a2'<- f a2
  return (Tuple a1' a2')

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple f (Quintuple a1 a2 a3 a4 a5) = do
  a1'<- f a1
  a2'<- f a2
  a3'<- f a3
  a4'<- f a4
  a5'<- f a5
  return (Quintuple a1' a2' a3' a4' a5')

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe _ Nothing = return Nothing
mapMMaybe f (Just a) = do
  a' <- f a
  return (Just a')

-- Ehhez a mellékhatásos mappoláshoz viszont a Monád megkötés sokat enged meg
-- A monád fő művelete a (>>=) :: m a -> (a -> m b) -> m b
-- Ami mellékhatásos műveletek közti függőséged modellez.
-- Viszont mappolásnál az egyes elemek között eredmény alapú függőség nincs
-- Applicative típusosztály: A Functor és a Monád között van
{-
:i Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘Control.Applicative’
-}
-- A típusosztály koncepciója az fmap művelet általánosítása tetszőleges paraméterű függvényre, pl.:
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- Ehhez viszont szükség van (egymástól független) mellékhatások kombinációjára
-- liftA műveletek csak liftA3-ig vannak standard libraryben, viszont arbitrary liftA írtható a <*> segítségével
-- pl.:
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 func fa fb fc = func <$> fa <*> fb <*> fc
-- func :: a -> b -> c -> d
-- func <$> fa :: f (b -> c -> d)
-- func <$> fa <*> fb :: f (c -> d)
-- func <$> fa <*> fb <*> fc :: f d
-- Ezeket az ún app láncot fogjuk használni mapA írásnál is!
-- Írjuk meg a mapM műveletet Applicative segítségével
-- Az algoritmus ugyanaz mint a funktornál csak függvényalkalmazás helyett <*> és független értékek esetén pure
mapA :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA _ Nil = pure Nil
mapA f (Cons a as) = Cons <$> f a <*> mapA f as

-- Ez a mappolhatósági tulajdonság lesz az úgynevezett Traversable típusosztály
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in ‘Data.Traversable’
-}

instance Traversable Single where
  traverse :: Applicative f => (a -> f b) -> Single a -> f (Single b)
  traverse f (Single a) = Single <$> f a

instance Traversable Tuple where
  traverse :: Applicative f => (a -> f b) -> Tuple a -> f (Tuple b)
  traverse f (Tuple a1 a2) = Tuple <$> f a1 <*> f a2

instance Traversable Quintuple where
  traverse :: Applicative f => (a -> f b) -> Quintuple a -> f (Quintuple b)
  traverse f (Quintuple a1 a2 a3 a4 a5) = Quintuple <$> f a1 <*> f a2 <*> f a3 <*> f a4 <*> f a5

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = Just <$> f a

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (Last a) = Last <$> f a
  traverse f (NECons a as) = NECons <$> f a <*> traverse f as

instance Traversable NonEmpty2 where
  traverse :: Applicative f => (a -> f b) -> NonEmpty2 a -> f (NonEmpty2 b)
  traverse f (NECons2 a as) = NECons2 <$> f a <*> traverse f as

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node as1 a as2) = Node <$> traverse f as1 <*> f a <*> traverse f as2

instance Traversable (Either fixed) where
  traverse :: Applicative f => (a -> f b) -> Either fixed a -> f (Either fixed b)
  traverse _ (Left fixed) = Left <$> pure fixed
  traverse f (Right a) = Right <$> f a

instance Traversable (BiTuple fixed) where
  traverse :: Applicative f => (a -> f b) -> BiTuple fixed a -> f (BiTuple fixed b)
  traverse f (BiTuple fixed a) = BiTuple <$> pure fixed <*> f a

instance Traversable (TriEither fixed1 fixed2) where
  traverse :: Applicative f => (a -> f b) -> TriEither fixed1 fixed2 a -> f (TriEither fixed1 fixed2 b)
  traverse f (LeftT fixed) = LeftT <$> pure fixed
  traverse f (MiddleT fixed) = MiddleT <$> pure fixed
  traverse f (RightT a) = RightT <$> f a

instance Traversable (BiList fixed) where
  traverse :: Applicative f => (a -> f b) -> BiList fixed a -> f (BiList fixed b)
  traverse f (ACons fixed as) = ACons <$> pure fixed <*> traverse f as
  traverse f (BCons a as) = BCons <$> f a <*> traverse f as

-- Magasabbrendű megkötések
instance Traversable fun => Traversable (Apply fun) where
  traverse :: (Applicative f, Traversable fun) => (a -> f b) -> Apply fun a -> f (Apply fun b)
  traverse f (MkApply fa) = MkApply <$> traverse f fa

instance Traversable fun => Traversable (Fix fun) where
  traverse :: (Applicative f, Traversable fun) => (a -> f b) -> Fix fun a -> f (Fix fun b)
  traverse f (MkFix fa) = MkFix <$> traverse (traverse f) fa

instance (Traversable fun, Traversable gun) => Traversable (Compose fun gun) where
  traverse :: (Applicative f, Traversable fun, Traversable gun) => (a -> f b) -> Compose fun gun a -> f (Compose fun gun b)
  traverse f (MkCompose fga) = MkCompose <$> traverse (traverse f) fga

instance Traversable fun => Traversable (Sum fun fixed) where
  traverse :: (Applicative f, Traversable fun) => (a -> f b) -> Sum fun fixed a -> f (Sum fun fixed b)
  traverse _ (FLeft ffixed) = FLeft <$> pure ffixed
  traverse f (FRight fa) = FRight <$> traverse f fa

instance Traversable fun => Traversable (Prod fun fixed) where
  traverse :: (Applicative f, Traversable fun) => (a -> f b) -> Prod fun fixed a -> f (Prod fun fixed b)
  traverse f (FProd ffixed fa) = FProd <$> pure ffixed <*> traverse f fa

instance Traversable fun => Traversable (FList fun) where
  traverse :: (Applicative f, Traversable fun) => (a -> f b) -> FList fun a -> f (FList fun b)
  traverse f (FNil) = pure FNil
  traverse f (FCons fa ffa) = FCons <$> traverse f fa <*> traverse (traverse f) ffa

-- Kiegészítő tananyag: Applicative Do

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
