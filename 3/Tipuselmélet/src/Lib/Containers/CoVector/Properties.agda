{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoVector.Properties where

open import Lib.Containers.CoVector.Type
open import Lib.Containers.CoVector.Base
open import Lib.Containers.CoVector.Bisimilarity
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.CoNat.Type
open import Lib.CoNat.Base renaming (_+_ to _+∞_)
open import Lib.CoNat.Literals
open import Lib.Nat.Type
open import Lib.Sigma.Type

open import Lib.Dec.Type
open import Lib.Dec.PatternSynonym

reflV : ∀{i}{A : Set i}{n : ℕ∞}(xs : CoVec A n) → xs ≈V xs
head-≡ (reflV xs) = refl
tail-≈ (reflV xs) = reflV (tail xs)

symV : ∀{i}{A : Set i}{n : ℕ∞}{xs ys : CoVec A n} → xs ≈V ys → ys ≈V xs
head-≡ (symV e) = sym (head-≡ e)
tail-≈ (symV e) = symV (tail-≈ e)

transV : ∀{i}{A : Set i}{n : ℕ∞}{xs ys zs : CoVec A n} → xs ≈V ys → ys ≈V zs → xs ≈V zs
head-≡ (transV e1 e2) = trans (head-≡ e1) (head-≡ e2)
tail-≈ (transV e1 e2) = transV (tail-≈ e1) (tail-≈ e2)

map-empty : ∀{a b}{A : Set a}{B : Set b}{f : A → B} →
            map f [] ≈V []
head-≡ map-empty ⦃ () ⦄
tail-≈ map-empty ⦃ () ⦄

replicate-0 : ∀{i}{A : Set i}{x : A} → replicate {i} {A} 0 x ≈V []
head-≡ replicate-0 ⦃ () ⦄
tail-≈ replicate-0 ⦃ () ⦄

CoVec-0-η : ∀{i}{A : Set i}(xs : CoVec A 0) → xs ≈V []
head-≡ (CoVec-0-η _) ⦃ () ⦄
tail-≈ (CoVec-0-η _) ⦃ () ⦄

CoVec-η : ∀{i}{A : Set i}{n : ℕ∞}{xs : CoVec A n} → head xs ∷ tail xs ≈V xs
head-≡ CoVec-η = refl
tail-≈ CoVec-η = reflV _

{-
++∞ : ∀{i}{A : Set i}{n : ℕ∞}{xs : CoVec A ∞}{ys : CoVec A n} →
  substCoVec (∞+n≡∞ n) (xs ++ ys) ≈V xs
head-≡ (++∞ {n = n} {xs} {ys}) = refl
tail-≈ (++∞ {n = n} {xs} {ys}) = -- ++∞ {xs = tail xs} {ys}
-}

finite-dec : ∀{i}{A : Set i}{n : ℕ} → 
  ⦃ DecidableEquality A ⦄ → (xs ys : CoVec A (embed n)) → Dec (xs ≈V ys)
finite-dec {n = zero} ⦃ e ⦄ xs ys = yes (transV (CoVec-0-η xs) (symV (CoVec-0-η ys)))
finite-dec {n = suc n} ⦃ DecProof e ⦄ xs ys with e (head xs) (head ys)
finite-dec {n = suc n} ⦃ DecProof e ⦄ xs ys | no p = no λ x → p (head-≡ x)
finite-dec {n = suc n} ⦃ pr@(DecProof e) ⦄ xs ys | yes p with finite-dec {n = n} ⦃ pr ⦄ (tail xs) (tail ys)
finite-dec {n = suc n} ⦃ DecProof e ⦄ xs ys | yes p | no q = no λ x → q (tail-≈ x)
finite-dec {n = suc n} ⦃ DecProof e ⦄ xs ys | yes p | yes q = yes (p ∷V q)
