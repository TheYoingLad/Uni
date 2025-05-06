{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoVector.Type where

open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.CoNat.Literals

infixr 5 _∷_
record CoVec {ℓ}(A : Set ℓ) (n : ℕ∞) : Set ℓ where
  coinductive
  constructor _∷_
  field
    head : .⦃ IsNotZero∞ n ⦄ → A
    tail : .⦃ p : IsNotZero∞ n ⦄ → CoVec A (predℕ∞ n)

open CoVec public

instance
  [] : ∀{i}{A : Set i} → CoVec A 0
  head [] ⦃ () ⦄
  tail [] ⦃ () ⦄
