{-# OPTIONS --safe --without-K #-}

module Lib.IndCoFin.Type where

open import Lib.CoNat.Type
open import Lib.CoNat.Base

data IndCoFin : ℕ∞ → Set where
  izero : {n : ℕ∞} → .⦃ IsNotZero∞ n ⦄ → IndCoFin n
  isuc  : {n : ℕ∞} → .⦃ p : IsNotZero∞ n ⦄ → IndCoFin (predℕ∞ n ⦃ p ⦄) → IndCoFin n
