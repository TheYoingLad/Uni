{-# OPTIONS --safe --without-K #-}

module Lib.Empty.Instances.DecidableEquality where

open import Lib.Empty.Type
open import Lib.Empty.Properties

open import Lib.Dec

instance
  DecEq⊥ : DecidableEquality ⊥
  DecEq⊥ = DecProof _≟_
