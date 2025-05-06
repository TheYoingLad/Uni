{-# OPTIONS --safe --without-K #-}

module Lib.IndCoFin.Literals where

open import Agda.Builtin.FromNat public
open import Lib.CoNat.Base using (IsNotZero∞; _ℕ<ℕ∞_)
open import Lib.IndCoFin.Type
open import Lib.IndCoFin.Base using (embed)

instance
  numICF : ∀ {n} → .⦃ IsNotZero∞ n ⦄ → Number (IndCoFin n)
  Number.Constraint (numICF {n}) k = k ℕ<ℕ∞ n
  Number.fromNat (numICF {n}) k ⦃ p ⦄ = embed k {n} p
