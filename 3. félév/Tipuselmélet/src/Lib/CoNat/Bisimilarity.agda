{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoNat.Bisimilarity where

open import Lib.CoNat.PatternSynonym
open import Lib.CoNat.Type
open import Lib.Maybe.Type
open import Lib.Unit.Type
open import Lib.Empty.Type
open import Lib.Equality.Type

infix 4 _≈ℕ∞_ _≈ℕ∞′_

record _≈ℕ∞_ (x y : ℕ∞) : Set

data _≈ℕ∞′_ : Maybe ℕ∞ → Maybe ℕ∞ → Set where
  instance nothing-refl : nothing ≈ℕ∞′ nothing
  cong-just : ∀{x y} → x ≈ℕ∞ y → just x ≈ℕ∞′ just y

record _≈ℕ∞_ x y where
  coinductive
  field prove : pred∞ x ≈ℕ∞′ pred∞ y

open _≈ℕ∞_ public
{-
infix 4 _≈ℕ∞ₚᵣ_ _≈ℕ∞′ₚᵣ_
record _≈ℕ∞ₚᵣ_ {a b c d : ℕ∞} (e1 : a ≈ℕ∞ b) (e2 : c ≈ℕ∞ d) : Set

data _≈ℕ∞′ₚᵣ_ {a b c d : Maybe ℕ∞} : (e1 : a ≈ℕ∞′ b) → (e2 : c ≈ℕ∞′ d) → Set where
_≈ℕ∞'ₚᵣ_ {zero∞} {zero∞} {zero∞} {zero∞} e1 e2 = ⊤
_≈ℕ∞'ₚᵣ_ {zero∞} {zero∞} {suc∞ c} {suc∞ d} e1 e2 = _≈ℕ∞ₚᵣ_ {conat' zero∞} {conat' zero∞} {c} {d} (λ where .prove → ?) e2
_≈ℕ∞'ₚᵣ_ {suc∞ a} {suc∞ b} {zero∞} {zero∞} e1 e2 = _≈ℕ∞ₚᵣ_ {a} {b} {conat' zero∞} {conat' zero∞} e1 λ where .prove → ?
_≈ℕ∞'ₚᵣ_ {suc∞ a} {suc∞ b} {suc∞ c} {suc∞ d} e1 e2 = _≈ℕ∞ₚᵣ_ {a} {b} {c} {d} e1 e2

record _≈ℕ∞ₚᵣ_ {a b c d} e1 e2 where
  coinductive
  field
    prove-eq : prove e1 ≈ℕ∞'ₚᵣ prove e2

open _≈ℕ∞ₚᵣ_ public
-}
