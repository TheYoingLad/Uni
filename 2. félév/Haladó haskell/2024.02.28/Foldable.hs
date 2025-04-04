{-# LANGUAGE InstanceSigs, QuantifiedConstraints, StandaloneDeriving #-}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..))

-- Hajtogatás: Listára való rekurzió szimulációja
-- foldr :: (a ->  b  ->  b ) ->  b -> [a] -> b
--   (:) :: (a -> [a] -> [a])
--
--                         [] :: [a]
-- A hajtogatás összekombinálja az összes 'a' típusú kifejezést egy listában.
-- Próbáljuk ezt meg más típussal is eljátszani:

data Single a = Single a deriving (Eq, Show)
data Tuple a = Tuple a a deriving (Eq, Show)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show)
data List a = Nil | Cons a (List a) deriving (Eq, Show)
data Maybe a = Just a | Nothing deriving (Eq, Show)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
data Either e a = Left e | Right a deriving (Eq, Show)
data BiTuple e a = BiTuple e a deriving (Eq, Show)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show)
data Apply f a = MkApply (f a) deriving (Eq, Show)
data Fix f a = MkFix (f (Fix f a))
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show)
data FList f a = FNil | FCons (f a) (f (FList f a))

foldrSingle :: (a -> b -> b) -> b -> Single a -> b
foldrSingle f b (Single a) = f a b

foldrTuple :: (a -> b -> b) -> b -> Tuple a -> b
foldrTuple f b (Tuple a1 a2)  = f a1 (f a2 b)

foldrQuintuple :: (a -> b -> b) -> b -> Quintuple a -> b
foldrQuintuple f b (Quintuple a1 a2 a3 a4 a5) = f a1 (f a2 (f a3 (f a4 (f a5 b))))

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList f b Nil = b
foldrList f b (Cons x xs) = f x (foldrList f b xs)

foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe f b Nothing = b
foldrMaybe f b (Just a) = f a b

-- Hasonlóan a mappolhatósághoz, a hajtogatás is általánosítható a Foldable típusosztály segítéségvel
{-
:i Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap' :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
    -- Defined in ‘Data.Foldable’
-}
-- A típusosztályban a többi művelet mind a foldr (vagy később a foldMap) segítségével
-- Például:
-- sum = foldr (+) 0
-- product = foldr (*) 1
-- toList = foldr (:) []
instance Foldable Single where
  foldr :: (a -> b -> b) -> b -> Single a -> b
  foldr = foldrSingle

  foldMap :: Monoid m => (a -> m) -> Single a -> m
  foldMap f (Single a) = f a

instance Foldable Tuple where
  foldr :: (a -> b -> b) -> b -> Tuple a -> b
  foldr = foldrTuple

  foldMap :: Monoid m => (a -> m) -> Tuple a -> m
  foldMap f (Tuple a1 a2) = f a1 <> f a2

instance Foldable Quintuple where
  foldr :: (a -> b -> b) -> b -> Quintuple a -> b
  foldr = foldrQuintuple

  foldMap :: Monoid m => (a -> m) -> Quintuple a -> m
  foldMap f (Quintuple a1 a2 a3 a4 a5) = f a1 <> f a2 <> f a3 <> f a4 <> f a5

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = foldrList

  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr = foldrMaybe

  foldMap :: Monoid m => (a -> m) -> Maybe a -> m
  foldMap f (Just a) = f a
  foldMap _ _ = mempty

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f b (Last a) = f a b
  foldr f b (NECons a as) = f a (foldr f b as)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (Last a) = f a
  foldMap f (NECons a as) = f a <> foldMap f as

instance Foldable NonEmpty2 where
  foldr :: (a -> b -> b) -> b -> NonEmpty2 a -> b
  foldr f b (NECons2 a as) = f a (foldr f b as)

  foldMap :: Monoid m => (a -> m) -> NonEmpty2 a -> m
  foldMap f (NECons2 a as) = f a <> foldMap f as

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b (Leaf a) = f a b
  foldr f b (Node as1 a as2) = foldr f (f a (foldr f b as2)) as1
  
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (Leaf a) = f a
  foldMap f (Node as1 a as2) = foldMap f as1 <> f a <> foldMap f as2

instance Foldable (Either fixed) where
  foldr :: (a -> b -> b) -> b -> Either fixed a -> b
  foldr f b (Right a) = f a b
  foldr f b _ = b

  foldMap :: Monoid m => (a -> m) -> Either fixed a -> m
  foldMap f (Left fixed) = mempty
  foldMap f (Right a) = f a

instance Foldable (BiTuple fixed) where
  foldr :: (a -> b -> b) -> b -> BiTuple fixed a -> b
  foldr f b (BiTuple fixed a) = f a b

  foldMap :: Monoid m => (a -> m) -> BiTuple fixed a -> m
  foldMap f (BiTuple fixed a) = f a

instance Foldable (TriEither fixed1 fixed2) where
  foldr :: (a -> b -> b) -> b -> TriEither fixed1 fixed2 a -> b
  foldr f b (RightT a) = f a b
  foldr f b _ = b

  foldMap :: Monoid m => (a -> m) -> TriEither fixed1 fixed2 a -> m
  foldMap f (LeftT fixed) = mempty
  foldMap f (MiddleT fixed) = mempty
  foldMap f (RightT a) = f a

instance Foldable (BiList fixed) where
  foldr :: (a -> b -> b) -> b -> BiList fixed a -> b
  foldr f b (ACons fixed as) = foldr f b as
  foldr f b (BCons a as) = f a (foldr f b as)
  foldr f b _ = b

  foldMap :: Monoid m => (a -> m) -> BiList fixed a -> m
  foldMap f (ACons fixed as) = foldMap f as
  foldMap f (BCons a as) = f a <> foldMap f as


-- Magasabbrendű megkötések
instance Foldable f => Foldable (Apply f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Apply f a -> b
  foldr f b (MkApply fa) = foldr f b fa

  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Apply f a -> m
  foldMap f (MkApply fa) = foldMap f fa

instance Foldable f => Foldable (Fix f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Fix f a -> b
  foldr f b (MkFix fa) = foldr (\ga b -> foldr f b ga) b fa
  
  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Fix f a -> m
  foldMap f (MkFix fa) = foldMap (foldMap f) fa

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr :: (Foldable f, Foldable g) =>(a -> b -> b) -> b -> Compose f g a -> b
  foldr f b (MkCompose fa) = foldr (\ga b -> foldr f b ga) b fa

  foldMap :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> Compose f g a -> m
  foldMap f (MkCompose fa) = foldMap (foldMap f) fa

instance Foldable f => Foldable (Sum f fixed) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Sum f fixed a -> b
  foldr f b (FLeft fixed) = b
  foldr f b (FRight fa) = foldr f b fa
  
  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Sum f fixed a -> m
  foldMap f (FRight fa) = foldMap f fa
  foldMap _ _ = mempty

instance Foldable f => Foldable (Prod f fixed) where
  foldr :: Foldable f => (a -> b -> b) -> b -> Prod f fixed a -> b
  foldr f b (FProd fixed fa) = foldr f b fa

  foldMap :: (Foldable f, Monoid m) => (a -> m) -> Prod f fixed a -> m
  foldMap f (FProd fixed fb) = foldMap f fb

instance Foldable f => Foldable (FList f) where
  foldr :: Foldable f => (a -> b -> b) -> b -> FList f a -> b
  foldr f b (FCons fa fas) = foldr f (foldr (\ga b -> foldr f b ga) b fas) fa
  foldr _ b _ = b

  foldMap :: (Foldable f, Monoid m) => (a -> m) -> FList f a -> m
  foldMap f (FCons fa fas) = foldMap f fa <> foldMap (foldMap f) fas

{-

Félcsoport: Olyan H halmaz, amely rendelkezik egy <> asszociatív művelettel
Ez Haskellben a Semigroup típusosztály

Írjunk Semigroup instance-ot az alábbi típusokra!

-}

instance Semigroup Bool where
  (<>) :: Bool -> Bool -> Bool
  (<>) = (&&)

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+)

data Endo a = MkEndo (a -> a)

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (MkEndo f) <> (MkEndo g) = MkEndo (f . g)

{-
Egy halmazhoz több művelet is választható, hogy félcsoportot alkossanak
Pl.:
(ℝ, +)         egy félcsoport
(ℝ, *)         egy félcsoport
(A, \x y -> x) egy félcsoport
(A, \x y -> y) egy félcsoport
-}

{-

Egységelemes félcsoport: Egy olyan félcsoport, amelynek van egy mempty egységeleme, azaz

mempty <> x = x <> mempty = x

Pl.:
(ℝ, +, 0)        egy egységelemes félcsoport
(ℝ, *, 1)        egy egységelemes félcsoport
(List A, ++, []) egy egységelemes félcsoport

Ez Haskellben a Monoid típusosztály
Írjunk a fent választott félcsoportnak az egységelemét
-}

instance Monoid Bool where
  mempty :: Bool
  mempty = True

instance Monoid Int where
  mempty :: Int
  mempty = 0

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = MkEndo id


-- A foldr művelet alternatívája: foldMap
-- foldMap :: Monoid m => (a -> m) -> f a -> m
-- A <> műveletet használja kombinálásra, pl.:
-- foldMap f [a,b,c ...] = f a <> f b <> f c <> ... <> mempty

-- Írd meg a fenti típusokra a foldMap műveletet is!
-- Érdemes belátni, hogy
-- foldMap f xs = foldr (\x a -> f x <> a) mempty xs
-- foldr f b xs = let (MkEndo g) = foldMap (\x -> MkEndo (\a -> f x a)) xs in g b


-- Gyakorlás:
-- Írj ezekre Foldable instance-ot

data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a) deriving (Eq, Show)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)
data Tree3 a = Leaf3 a | Node3 (Tree3 a) (Tree3 a) deriving (Eq, Show)
data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
data Triplet a b c = Triplet a b c deriving (Eq, Show)
data SplitTree a b = SplitTree (Tree a) a b (Tree b) deriving (Eq, Show)
data TriCompose f g h a = TriCompose (f (g (h a))) deriving (Eq, Show)
data Free f a = Pure a | Free (f (Free f a))

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)

instance Foldable Tree2 where
  foldr f b (Node2 as1 a as2) = foldr f (f a (foldr f b as2)) as1
  foldr _ b _ = b

  foldMap f (Node2 as1 a as2) = foldMap f as1 <> f a <> foldMap f as2
  foldMap f _ = mempty

instance Foldable RoseTree where
  --foldr f b (RoseLeaf a) = f a b
  --foldr f b (RoseNode []) = b
  --foldr f b (RoseNode (a:as)) = foldr  (f f _) b [foldr f b a | a<-as]

  --foldMap f (RoseLeaf a) = f a
  --foldMap f (RoseNode as) = [foldMap f a | a <- as]

instance Foldable Tree3 where
  foldr = undefined

  foldMap f (Leaf3 a) = f a
  foldMap f (Node3 as1 as2) = foldMap f as1 <> foldMap f as2


instance Foldable SkipList where
  foldr = undefined

  foldMap f (Skip as) = foldMap f as
  foldMap f (SCons a as) = f a <> foldMap f as

instance Foldable CrazyType where
  foldr = undefined

  foldMap f (C1 a1 a2) = f a1 <> f a2
  foldMap f (C2 a _) = f a
  foldMap f (C3 as) = foldMap f as

instance Foldable (Either3 fixed1 fixed2)where
  foldr = undefined

  foldMap f (Left3 fixed) = mempty
  foldMap f (Middle3 fixed) = mempty
  foldMap f (Right3 a) = f a

instance Foldable (Triplet fixed1 fixed2) where
  foldr = undefined

  foldMap f (Triplet _ _ a) = f a

instance Foldable (SplitTree fixed) where
  foldr = undefined

  foldMap f (SplitTree fixeds1 fixed a as) = f a <> foldMap f as

instance (Foldable f, Foldable g, Foldable h) => Foldable (TriCompose f g h) where
  foldr = undefined

  foldMap f (TriCompose fgha) = foldMap (foldMap (foldMap f)) fgha

instance (Foldable f) => Foldable (Free f) where
  foldr = undefined

  foldMap f (Pure a) = f a
  foldMap f (Free fa) = foldMap (foldMap f) fa