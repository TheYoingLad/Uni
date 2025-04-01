{-# OPTIONS --safe --without-K #-}

module Lib.Fin.Instances.DecidableEquality where

open import Lib.Dec
open import Lib.Fin.Type
open import Lib.Fin.Properties

instance
  DecEqFin : ∀{n} → DecidableEquality (Fin n)
  DecEqFin = DecProof _≟_
