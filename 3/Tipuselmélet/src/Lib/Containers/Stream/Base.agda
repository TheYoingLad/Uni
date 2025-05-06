{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.Stream.Base where

open import Lib.Containers.Stream.Type
open import Lib.Containers.List.Type
open import Lib.Containers.Vector.Type
open import Lib.Sigma.Type
open import Lib.Nat.Type
open import Lib.Level
open import Lib.Dec.Type
open import Lib.Sum.Type

record All {i j}{A : Set i}(P : A → Set j)(s : Stream A) : Set (i ⊔ j) where
  coinductive
  field
    head-All : P (head s)
    tail-All : All P (tail s)

open All public

record Any {i j}{A : Set i}(P : A → Set j)(s : Stream A) : Set (i ⊔ j) where
  coinductive
  field
    headTail-Any : P (head s) ⊎ Any P (tail s)

open Any public

repeat : ∀{i}{A : Set i} → A → Stream A
head (repeat x) = x
tail (repeat x) = repeat x

infixr 5 _++_
_++_ : ∀{i}{A : Set i} → List A → Stream A → Stream A
[] ++ s = s
(x ∷ xs) ++ s = x ∷ xs ++ s

coiteStream : ∀{i j}{A : Set i}{B : Set j} → (A → A × B) → A → Stream B
head (coiteStream next seed) = snd (next seed)
tail (coiteStream next seed) = coiteStream next (fst (next seed))

iterate : ∀{i}{A : Set i} → (A → A) → A → Stream A
head (iterate f a) = a
tail (iterate f a) = iterate f (f a)

[0-∞] : Stream ℕ
[0-∞] = iterate suc 0

infixl 10 _‼_
_‼_ : ∀{i}{A : Set i} → Stream A → ℕ → A
xs ‼ zero    = head xs
xs ‼ (suc n) = (tail xs) ‼ n

map : ∀{i j}{A : Set i}{B : Set j} → (A → B) → Stream A → Stream B
head (map f s) = f (head s)
tail (map f s) = map f (tail s)

zipWith : ∀{i j k}{A : Set i}{B : Set j}{C : Set k} → (A → B → C) → Stream A → Stream B → Stream C
head (zipWith f s t) = f (head s) (head t)
tail (zipWith f s t) = zipWith f (tail s) (tail t)

splitAt : ∀{i}{A : Set i}(n : ℕ) → Stream A → Vec A n × Stream A
splitAt zero s = [] , s
splitAt (suc n) s = let (x , y) = splitAt n (tail s) in (head s ∷ x , y)

take : ∀{i}{A : Set i}(n : ℕ) → Stream A → Vec A n
take n s = fst (splitAt n s)

drop : ∀{i}{A : Set i}(n : ℕ) → Stream A → Stream A
drop n s = snd (splitAt n s)
