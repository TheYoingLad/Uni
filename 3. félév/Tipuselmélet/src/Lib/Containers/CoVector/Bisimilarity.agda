{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.Containers.CoVector.Bisimilarity where

open import Lib.Containers.CoVector.Type
open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.Equality.Type

infixr 5 _∷V_
infix 4 _≈V_
record _≈V_ {a} {A : Set a} {n : ℕ∞} (xs ys : CoVec A n) : Set a where
  constructor _∷V_
  coinductive
  field
    head-≡ : .⦃ e : IsNotZero∞ n ⦄ → head xs ≡ head ys
    tail-≈ : .⦃ e : IsNotZero∞ n ⦄ → tail xs ≈V tail ys

open _≈V_ public
