{-# OPTIONS --safe --guardedness --without-K #-}

module Lib.CoFin.Type where

open import Lib.CoNat.Base
open import Lib.CoNat.Literals
open import Lib.CoNat.Type
open import Lib.Maybe.Type
open import Lib.Empty.Type

open import Lib.Equality.Type

record CoFin (n : ℕ∞) : Set where
  coinductive
  constructor cofin
  field
    ⦃ inz ⦄ : IsNotZero∞ n
    fpred∞ : Maybe (CoFin (predℕ∞ n))

open CoFin public

coz : CoFin 0 → ⊥
coz = inz

proba : (n : ℕ∞) → .⦃ p1 p2 : IsNotZero∞ n ⦄ → CoFin n
proba n = cofin ⦃ recomputeIsNotZero∞ {n} ⦄ {- Az instance paramétert még valahogy el kéne tűntetni. -} nothing
