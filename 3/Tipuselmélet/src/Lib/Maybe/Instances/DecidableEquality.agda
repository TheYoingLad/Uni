{-# OPTIONS --safe --without-K #-}

module Lib.Maybe.Instances.DecidableEquality where

open import Lib.Maybe.Type
open import Lib.Maybe.Properties
open import Lib.Dec

instance
  DecEqMaybe : ∀{i}{A : Set i} → ⦃ DecidableEquality A ⦄ → DecidableEquality (Maybe A)
  DecEqMaybe {i} {A} ⦃ inst ⦄ = DecProof (≡-dec-Maybe {i} {A} (decide inst))
