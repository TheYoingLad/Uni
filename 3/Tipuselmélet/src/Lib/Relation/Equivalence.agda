{-# OPTIONS --safe --without-K #-}
module Lib.Relation.Equivalence where

open import Lib.Relation.Base
open import Agda.Primitive

record Equivalence {ℓ κ}{A : Set ℓ}(R : A → A → Set κ) : Set (ℓ ⊔ κ) where
  field
    transitivity : Transitive R
    reflexivity  : Reflexive R
    symmetry     : Symmetric R

open Equivalence ⦃ ... ⦄ public
