{-# OPTIONS --safe --without-K --guardedness #-}

module Lib.Containers.CoList.Properties where

open import Lib.Containers.CoList.Type
open import Lib.Containers.CoList.Base
open import Lib.Containers.CoList.Bisimilarity
open import Lib.Containers.CoList.PatternSynonym
open import Lib.Maybe.Type
open import Lib.Equality.Type
open import Lib.Equality.Base
open import Lib.Sigma.Type
open import Lib.CoNat.Type
open import Lib.CoNat.PatternSynonym
open import Lib.CoNat.Bisimilarity

reflL : ∀{i}{A : Set i}(xs : CoList A) → xs ≈L xs
reflL′ : ∀{i}{A : Set i}(xs : Maybe (A × CoList A)) → xs ≈L′ xs
reflL′ []∞ = nothing-refl
reflL′ (x ∷∞ xs) = cong-just-cons refl (reflL xs)
prove (reflL xs) = reflL′ (uncons xs)

symL : ∀{i}{A : Set i}{xs ys : CoList A} → xs ≈L ys → ys ≈L xs
symL′ : ∀{i}{A : Set i}{xs ys : Maybe (A × CoList A)} → xs ≈L′ ys → ys ≈L′ xs
symL′ {xs = []∞} {[]∞} e = nothing-refl
symL′ {xs = x ∷∞ xs} {y ∷∞ ys} (cong-just-cons xy xsys) = cong-just-cons (sym xy) (symL xsys)
prove (symL e) = symL′ (prove e)

transL : ∀{i}{A : Set i}{xs ys zs : CoList A} → xs ≈L ys → ys ≈L zs → xs ≈L zs
transL′ : ∀{i}{A : Set i}{xs ys zs : Maybe (A × CoList A)} → xs ≈L′ ys → ys ≈L′ zs → xs ≈L′ zs
transL′ {xs = []∞} {[]∞} {zs} e1 e2 = e2
transL′ {xs = x ∷∞ xs} {y ∷∞ ys} {z ∷∞ zs} (cong-just-cons e1 e2) (cong-just-cons e3 e4) = cong-just-cons (e1 ◾ e3) (transL e2 e4)
prove (transL e1 e2) = transL′ (prove e1) (prove e2)

CoList-η : ∀{i}{A : Set i}(xs : CoList A) → mkCoList (uncons xs) ≈L xs
prove (CoList-η xs) = reflL′ (uncons xs)

length-coreplicate : ∀{i}{A : Set i}(n : ℕ∞)(xs : CoList A) → 
  length (coreplicate n xs) ≈ℕ∞ n
prove (length-coreplicate n xs) with pred∞ n
... | zero∞  = nothing-refl
... | suc∞ b = cong-just (length-coreplicate b xs)

length-repeat-∞ : ∀{i}{A : Set i}(xs : CoList A) → length (repeat xs) ≈ℕ∞ ∞
length-repeat-∞ = length-coreplicate ∞
