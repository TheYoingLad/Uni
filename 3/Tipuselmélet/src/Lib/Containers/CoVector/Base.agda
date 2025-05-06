{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoVector.Base where

open import Lib.Containers.CoVector.Type
open import Lib.CoNat.Type
open import Lib.CoNat.Base
  renaming (_+_ to _+∞_)
open import Lib.CoNat.Properties
open import Lib.CoNat.PatternSynonym
open import Lib.CoNat.Bisimilarity

open import Lib.Nat.Type

open import Lib.Unit.Type

open import Lib.Maybe.Type

open import Lib.Containers.Vector.Type
  renaming ([] to []ᵥ ; _∷_ to _∷ᵥ_)

replicate : ∀{i}{A : Set i} → (n : ℕ∞) → A → CoVec A n
head (replicate n a) = a
tail (replicate n a) = replicate (predℕ∞ n) a

repeat : ∀{i}{A : Set i} → A → CoVec A ∞
repeat = replicate ∞

map : ∀{i j}{A : Set i}{B : Set j}{n : ℕ∞} → 
            (A → B) → CoVec A n → CoVec B n
head (map f xs) = f (head xs)
tail (map f xs) = map f (tail xs)

cast : ∀{i}{A : Set i}{n m : ℕ∞} → .(n ≈ℕ∞ m) → CoVec A n → CoVec A m
head (cast {n = n} {m} e xs) with pred∞ n | pred∞ m | .(prove e) | head xs
... | suc∞ n' | suc∞ m' | p | hxs = hxs
tail (cast {n = n} {m} e xs) with pred∞ n | pred∞ m | .(prove e) | (λ x → tail xs ⦃ x ⦄)
... | suc∞ n' | suc∞ m' | p | txs = cast (just-injectiveℕ∞ p) (txs tt)

infixr 5 _++_
_++_ : ∀{i}{A : Set i}{n m : ℕ∞} → CoVec A n → CoVec A m → CoVec A (n +∞ m)
head (_++_ {n = n} {m} xs ys) with pred∞ n in eq1
head (_++_ {n = n} {m} xs ys) | suc∞ x = head xs ⦃ JustIsNotZero∞′ {n} ⦃ eq1 ⦄ ⦄
head (_++_ {n = n} {m} xs ys) | zero∞ with pred∞ m in eq2
head (_++_ {n = n} {m} xs ys) | zero∞ | suc∞ m' = head ys ⦃ JustIsNotZero∞′ {m} ⦃ eq2 ⦄ ⦄
tail (_++_ {n = n} {m} xs ys) with pred∞ n | (λ x → tail xs ⦃ x ⦄)
tail (_++_ {n = n} {m} xs ys) | suc∞ x | tx = tx tt ++ ys
tail (_++_ {n = n} {m} xs ys) | zero∞ | _ with pred∞ m | (λ y → tail ys ⦃ y ⦄)
tail (_++_ {n = n} {m} xs ys) | zero∞ | _ | suc∞ m' | ty = cast (symℕ∞ (+-aux-inr m m')) (ty tt)

take∞ : ∀{i}{A : Set i}(n : ℕ∞){m : ℕ∞} → CoVec A (n +∞ m) → CoVec A n
head (take∞ n {m} xs) ⦃ e ⦄ with pred∞ n | weakenℕ∞ n m e | head xs
... | zero∞  | p | x = x ⦃ p ⦄
... | suc∞ _ | _ | x = x
tail (take∞ n {m} xs) with pred∞ n | (λ x → tail xs ⦃ x ⦄)
tail (take∞ n {m} xs) | suc∞ n' | t = take∞ n' {m} (t tt)

take : ∀{i}{A : Set i}(n : ℕ){m : ℕ∞} → .⦃ n ℕ≤ℕ∞ m ⦄ → CoVec A m → Vec A n
take zero v = []ᵥ
take (suc n) {m} v with pred∞ m | head v | (λ x → tail v ⦃ x ⦄)
... | just m' | hv | tv = hv ∷ᵥ take n {m'} (tv tt)
