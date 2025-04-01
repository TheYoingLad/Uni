{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Instances.DecidableEquality where

open import Lib.Nat.Properties using (_≟_)
open import Lib.Dec
open import Lib.Nat.Type

instance
  DecEqℕ : DecidableEquality ℕ
  DecEqℕ = DecProof _≟_
