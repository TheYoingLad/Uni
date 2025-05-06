{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoFin.Bisimilarity where

open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.Maybe.Type
open import Lib.CoFin.Type
open import Lib.Unit.Type
open import Lib.Empty.Type

infix 4 _≈CoFin_ _≈CoFin′_
record _≈CoFin_ {n : ℕ∞} (k l : CoFin n) : Set

data _≈CoFin′_ {n : ℕ∞} .⦃ e : IsNotZero∞ n ⦄ : Maybe (CoFin (predℕ∞ n)) → Maybe (CoFin (predℕ∞ n)) → Set where
  instance nothing-refl : nothing ≈CoFin′ nothing
  cong-just             : ∀{x y} → x ≈CoFin y → just x ≈CoFin′ just y

record _≈CoFin_ {n} k l where
  coinductive
  field
    prove : _≈CoFin′_ {n} ⦃ inz k ⦄ (fpred∞ k) (fpred∞ l)

open _≈CoFin_ public
