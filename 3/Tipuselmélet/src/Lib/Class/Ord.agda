{-# OPTIONS --safe --without-K #-}

module Lib.Class.Ord where

open import Lib.Level
open import Lib.Bool.Type
open import Lib.Class.Eq
open import Lib.Ordering.Type
open import Lib.Equality.Type
open import Lib.Sigma.Type

record Ord {i}(A : Set i) : Set (lsuc i) where
  constructor OrdInstance
  field
    overlap ⦃ eq ⦄ : Eq A
    compare : A → A → Ordering
    flippable : {x y : A} → compare x y ≡ LT ↔ compare y x ≡ GT
    equality : {x : A} → compare x x ≡ EQ
    consistencyWithEq : {x y : A} → x ≡ⁱ y → compare x y ≡ EQ

  infix 4 _<=_ _<_ _>=_ _>_
  _<=_ : A → A → Bool
  x <= y with compare x y
  ... | LT = true
  ... | EQ = true
  ... | GT = false

  _<_ : A → A → Bool
  x < y with compare x y
  ... | LT = true
  ... | EQ = false
  ... | GT = false

  _>=_ : A → A → Bool
  x >= y with compare x y
  ... | LT = false
  ... | EQ = true
  ... | GT = true

  _>_ : A → A → Bool
  x > y with compare x y
  ... | LT = false
  ... | EQ = false
  ... | GT = true

open Ord {{...}} public
