{-# OPTIONS --safe --without-K #-}
module Lib.Relation.PartialOrder where

open import Lib.Relation.Base
open import Agda.Primitive


record WeakPartialOrder {ℓ κ}{A : Set ℓ}(R : A → A → Set κ) : Set (ℓ ⊔ κ) where
  field
    reflexivity  : Reflexive R
    antisymmetry : AntiSymmetric R
    transitivity : Transitive R

open WeakPartialOrder ⦃ ... ⦄ public

record StrongPartialOrder {ℓ κ}{A : Set ℓ}(R : A → A → Set κ) : Set (ℓ ⊔ κ) where
  field
    irreflexivity : Irreflexive R
    asymmetry     : Asymmetric R
    transitivity : Transitive R

open StrongPartialOrder ⦃ ... ⦄ public
