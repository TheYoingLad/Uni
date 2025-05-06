{-# OPTIONS --safe --without-K #-}

module Lib.Unit.Instances.DecidableEquality where

open import Lib.Unit.Type
open import Lib.Unit.Properties
open import Lib.Dec

instance
  DecEq⊤ : DecidableEquality ⊤
  DecEq⊤ = DecProof _≟_
