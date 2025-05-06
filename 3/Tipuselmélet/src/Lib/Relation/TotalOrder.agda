{-# OPTIONS --safe --without-K #-}
module Lib.Relation.TotalOrder where

open import Lib.Relation.Base
open import Lib.Sum.Base
open import Lib.Function.Base
open import Lib.Empty.Base
open import Lib.Relation.PartialOrder renaming (reflexivity to reflPO; antisymmetry to antisymPO; transitivity to transPO)
open import Agda.Primitive

record TotalOrder {ℓ κ}{A : Set ℓ}(R : A → A → Set κ) : Set (ℓ ⊔ κ) where
  field
    reflexivity : Reflexive R
    transitivity : Transitive R
    antisymmetry : AntiSymmetric R
    total : StronglyConnected R

open TotalOrder ⦃ ... ⦄ public

instance
  total→partial : ∀{ℓ κ}{A : Set ℓ}{R : A → A → Set κ} → ⦃ r : TotalOrder R ⦄ → WeakPartialOrder R
  total→partial .reflPO = reflexivity
  total→partial .antisymPO = antisymmetry
  total→partial .transPO = transitivity

total→¬strong : ∀{ℓ κ}{A : Set ℓ}{R : A → A → Set κ} → ⦃ r : TotalOrder R ⦄ → ⦃ s : StrongPartialOrder R ⦄ → ¬ A
total→¬strong a = irreflexivity (reduceIdem $ total {a = a})
