{-# OPTIONS --safe --without-K #-}

module Lib.Sum.Instances.DecidableEquality where

open import Lib.Sum.Type
open import Lib.Dec
open import Lib.Sum.Properties

instance
  DecEq⊎ : ∀{i j}{A : Set i}{B : Set j} → ⦃ DecidableEquality A ⦄ → ⦃ DecidableEquality B ⦄ → DecidableEquality (A ⊎ B)
  DecEq⊎ ⦃ i1 ⦄ ⦃ i2 ⦄ = DecProof (dec-⊎ (decide i1) (decide i2))
