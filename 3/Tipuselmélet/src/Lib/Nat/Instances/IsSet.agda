{-# OPTIONS --safe --without-K #-}

module Lib.Nat.Instances.IsSet where

open import Lib.Class.IsSet
open import Lib.Nat.Type
open import Lib.Nat.Instances.DecidableEquality
open import Lib.Dec.InstanceGenerators.IsSet

instance
  IsSetℕ : IsSet ℕ
  IsSetℕ = DecidableEquality→IsSet DecEqℕ
