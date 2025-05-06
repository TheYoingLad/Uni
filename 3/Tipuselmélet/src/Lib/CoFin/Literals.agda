{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoFin.Literals where

open import Agda.Builtin.FromNat public
open import Lib.CoFin.Type
open import Lib.CoNat.Type
open import Lib.CoNat.Base
open import Lib.CoNat.Properties
open import Lib.Nat.Type
open import Lib.Maybe.Type

instance
  NumCoFin : {n : ℕ∞} → .⦃ IsNotZero∞ n ⦄ → Number (CoFin n)
  Number.Constraint (NumCoFin {n}) k = k ℕ<ℕ∞ n
  Number.fromNat (NumCoFin {n} ⦃ e ⦄) zero ⦃ inst ⦄ = cofin ⦃ recomputeIsNotZero∞ {n} ⦄ nothing
  inz (Number.fromNat (NumCoFin {n} ⦃ e ⦄) (suc k) ⦃ inst ⦄) = recomputeIsNotZero∞ {n}
  fpred∞ (Number.fromNat (NumCoFin {n} ⦃ e ⦄) (suc k) ⦃ inst ⦄) with pred∞ n
  ... | just x = just (fromNat ⦃ NumCoFin {x} ⦃ <→IsNotZero∞ {k} {x} ⦄ ⦄ k ⦃ inst ⦄ )
